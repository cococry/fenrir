#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdbool.h>
#include <unistd.h>

enum token_type_t {
  TOK_START_TAG,
  TOK_END_TAG,
  TOK_TEXT,
  TOK_EOF
};

enum lexer_state_t {
  LEX_TEXT,
  LEX_TAG_OPEN,
  LEX_TAG_END,
  LEX_TAG_NAME,
  LEX_ATTRS
};

#define MAX_ATTR_LEN 256
#define INIT_CHILD_CAP 8

struct token_t {
  enum token_type_t type;
  char* tag_name;
  char* text;

  char** attr_names, **attr_vals;
  uint32_t n_attrs;
};

struct lexer_t {
  enum lexer_state_t state;

  char* tmp;
  size_t tmp_len;

  struct token_t* toks;
  uint32_t n_toks;
};

enum node_type_t {
  NODE_NONE = 0,
  NODE_ROOT,
  NODE_H1, 
  NODE_P, 
  NODE_DIV,
  NODE_LINK
};

struct node_t {
  struct node_t** childs;
  struct node_t* parent;

  uint32_t cap_childs, num_childs;

  char* text;
  char* tag_name;

  enum node_type_t type;

  uint32_t text_level; 
};

void emit_token(
  struct lexer_t* lexer, enum token_type_t type
) {
  if(!lexer->tmp_len) return;

  lexer->tmp[lexer->tmp_len] = '\0';

  lexer->toks[lexer->n_toks++] = (struct token_t){
    .type = type,
    .text = type == TOK_TEXT ? strdup(lexer->tmp) : NULL,
    .tag_name = type != TOK_TEXT ? strdup(lexer->tmp) : NULL,
  };

  lexer->tmp_len  = 0;
  lexer->tmp[lexer->tmp_len] = '\0';
}

void append_to_buffer(struct lexer_t* lexer, char c) {
  lexer->tmp[lexer->tmp_len++] = c;
}
void init_lexer(struct lexer_t* lexer) {
  memset(lexer, 0, sizeof(*lexer));
  lexer->toks = malloc(sizeof(*lexer->toks) * 1024);
  lexer->tmp = malloc(1024);
}

struct node_t* create_node(enum node_type_t type) {
  struct node_t* node = (struct node_t*)malloc(sizeof(struct node_t));
  memset(node, 0, sizeof(struct node_t));
  node->parent = NULL;
  node->cap_childs = INIT_CHILD_CAP;
  node->num_childs = 0;
  node->childs = malloc(sizeof(struct node_t) * INIT_CHILD_CAP);
  node->type = type;

  return node;
}

void
node_add_child(struct node_t* parent, struct node_t* child) {
  if(!parent || !child) return;

  if(parent->num_childs >= parent->cap_childs) {
    uint32_t new_cap = parent->cap_childs == 0 ? 2 : parent->cap_childs * 2;
    parent->childs = (struct node_t**)realloc(parent->childs, new_cap * sizeof(struct node_t));
    parent->cap_childs = new_cap;
  }

  parent->childs[parent->num_childs++] = child;
  child->parent = parent;

  return;
}

struct node_t*
dom_pop(struct node_t* ptr) {
  if (!ptr || !ptr->parent)
    return ptr;
  return ptr->parent;
}


enum node_type_t tagname_to_node_type(const char* tagname) {
  if(strcmp(tagname, "h1") == 0) {
    return NODE_H1;
  } else if(strcmp(tagname, "div") == 0) {
    return NODE_DIV;
  } else if(strcmp(tagname, "a") == 0) {
    return NODE_LINK;
  } else if(strcmp(tagname, "p") == 0) {
    return NODE_P;
  }
  return NODE_NONE;

}

uint32_t get_attrs(char c, uint32_t i, const char* body, uint32_t* o_n_attr, char** attr_keys, char** attr_vals) {
  bool count_only = !attr_keys && !attr_vals && o_n_attr;

  uint32_t attr = 0;
  while(c && c != '>') {
    uint32_t val_i = 0, name_i = 0;

    // skip whitespace
    while(c && isspace(c)) { i++; c = body[i]; }

    // key declaration is until next ' ' or = 
    while(c && c != '=' && !isspace(c) && c != '>') {
      if(!count_only && attr_keys && name_i + 1 < MAX_ATTR_LEN) attr_keys[attr][name_i++] = c;

      i++;
      c = body[i];
    }

    // terminate key
    if(!count_only && attr_keys && name_i + 1 < MAX_ATTR_LEN) attr_keys[attr][name_i] = '\0';

    // skip whitespace
    while(c && isspace(c)) { i++; c = body[i]; }

    if(c == '=') {
      // skip '=' 
      i++;
      c = body[i];

      // skip whitespace
      while(c && isspace(c)) { i++; c = body[i]; }

      bool instr = false;
      while(c && c != '>') {
        bool quote = c == '\'' || c == '\"';
        if(quote) {
          instr = !instr;
          i++;
          c = body[i];
          continue; 
        }

        if(!count_only && attr_vals && val_i + 1 < MAX_ATTR_LEN) attr_vals[attr][val_i++] = c;

        if(!instr && isspace(c)) break;

        i++;
        c = body[i];
      }

      // terminate value 
      if(!count_only && attr_vals && val_i + 1 < MAX_ATTR_LEN) attr_vals[attr][val_i] = '\0';

    } else if(!count_only && attr_vals) {
      attr_vals[attr][0] = '\0';
    }
    attr++;
  }

  if(o_n_attr) *o_n_attr = attr;

  return i;
}

void lex(char* body, struct lexer_t* lexer) {
  enum lexer_state_t state = LEX_TEXT;

  size_t i = 0;
  char attr_name[1024];
  char attr_val[1024];
  uint32_t attr_name_i = 0, attr_val_i  = 0;
  while (body[i] != '\0') {
    char c = body[i];

    switch (state) {
      case LEX_TEXT:
        if (c == '<') {
          emit_token(lexer, TOK_TEXT);
          state = LEX_TAG_OPEN;
        } else {
          append_to_buffer(lexer, c);
        }
        break;

      case LEX_TAG_OPEN:
        if (c == '/') {
          state = LEX_TAG_END;
        } else {
          state = LEX_TAG_NAME;
          append_to_buffer(lexer, c);
        }
        break;

      case LEX_TAG_NAME:
        if (c == '>' || isspace(c)) {

          state = (c == '>') ? LEX_TEXT : LEX_ATTRS;
          if(state == LEX_TEXT) {
            emit_token(lexer, TOK_START_TAG);
          }
        } else {
          append_to_buffer(lexer, c);
        }
        break;

      case LEX_TAG_END:
        if (c == '>') {
          emit_token(lexer, TOK_END_TAG);
          state = LEX_TEXT;

        } else {
          append_to_buffer(lexer, c);
        }
        break;

      case LEX_ATTRS: 
        while(c == ' ') i++;
        c = body[i];

        uint32_t n_attr = 0;
        get_attrs(c, i, body, &n_attr, NULL, NULL); 

        char** attr_keys = malloc(sizeof(char*) * n_attr);
        for(uint32_t i = 0; i < n_attr; i++) attr_keys[i] = malloc(MAX_ATTR_LEN);

        char** attr_vals = malloc(sizeof(char*) * n_attr);
        for(uint32_t i = 0; i < n_attr; i++) attr_vals[i] = malloc(MAX_ATTR_LEN);

        i = get_attrs(c, i, body, NULL, attr_keys, attr_vals);

        lexer->tmp[lexer->tmp_len] = '\0';

        lexer->toks[lexer->n_toks++] = (struct token_t){
          .type = TOK_START_TAG,
          .tag_name = strdup(lexer->tmp),
          .attr_names = attr_keys,
          .attr_vals = attr_vals,
          .n_attrs = n_attr 
        };

        lexer->tmp_len  = 0;
        lexer->tmp[lexer->tmp_len] = '\0';

        attr_val_i = 0;
        attr_name_i = 0;

        state = LEX_TEXT; 

        break;
    }
    i++;
  }
}

char *decode_chunked(char *body, size_t *out_len) {
  char *out = NULL;
  size_t total = 0;
  while (true) {
    unsigned chunk_size;
    sscanf(body, "%x", &chunk_size);
    if (chunk_size == 0) break;

    // chunks are seperated by \r\n, chunk position is after \r\n
    body = strstr(body, "\r\n") + 2;

    // alloc mem for the new chunk
    char *tmp = realloc(out, total + chunk_size + 1);
    out = tmp;

    // copy chunk data
    memcpy(out + total, body, chunk_size);
    total += chunk_size;

    // skip data + CRLF
    body += chunk_size + 2; 
  }

  out[total] = '\0';
  *out_len = total;
  return out;
}

int is_chunked(const char *response) {
  return strstr(response, "Transfer-Encoding: chunked") != NULL;
}


void print_tree(const struct node_t *node, int depth) {
  if (!node) return;



  if(node->type != NODE_NONE && node->tag_name) {
    printf("(%i) %s%s",  node->num_childs,node->tag_name, node->text ? ": " : "\n");
    if(node->text) {
      printf("%s\n", node->text);
    }
    if(node->num_childs)
      printf("Children: \n");
  }

  // recurse into children
  for (size_t i = 0; i < node->num_childs; i++) {
    print_tree(node->childs[i], node->type != NODE_NONE ? depth + 1 : depth);
  }
}

int main(void) {
  struct addrinfo hints;
  struct addrinfo* res; 


  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  int ecode = getaddrinfo("example.com", "80", &hints, &res);

  if(ecode != 0){
    fprintf(stderr, "Failed to fetch URL IP: %s\n", gai_strerror(ecode)); 
    return 1;
  }

  int sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  if(connect(sock, res->ai_addr, res->ai_addrlen) != 0) {
    fprintf(stderr, "Failed to connect to socket domain: %s\n", strerror(errno)); 
    return 1;
  }

  const char* req = 
    "GET / HTTP/1.1\r\n"
    "Host: example.com\r\n"
    "Connection: close\r\n"
    "\r\n";
  send(sock, req, strlen(req), 0);

  char *response = NULL;
  size_t total = 0;

  char buffer[4096];
  ssize_t bytes;

  while ((bytes = recv(sock, buffer, sizeof(buffer), 0)) > 0) {
    char *tmp = realloc(response, total + bytes + 1);
    if (!tmp) {
      free(response);
      perror("realloc");
      return 1;
    }
    response = tmp;
    memcpy(response + total, buffer, bytes);
    total += bytes;
  }

  response[total] = '\0';

  char *body = strstr(response, "\r\n\r\n");

  body += 4;
  size_t html_len;

  if(is_chunked(response)) {
    char *html = decode_chunked(body, &html_len);

    if (!html) {
      fprintf(stderr, "Chunk decoding failed\n");
      return 1;
    }

    struct lexer_t lexer;
    init_lexer(&lexer);
    lex(html, &lexer);

    struct node_t* root = create_node(NODE_ROOT); 

    struct node_t* ptr = root; 
    for(uint32_t i = 0; i < lexer.n_toks; i++) {
      struct token_t t = lexer.toks[i];

      switch (t.type) {
        case TOK_TEXT: {
          if (!ptr->text) {
            ptr->text = malloc(strlen(t.text) + 1);
            strcpy(ptr->text, t.text);
          } else {
            ptr->text = realloc(ptr->text,
                                strlen(ptr->text) + strlen(t.text) + 1);
            strcat(ptr->text, t.text);
          }
          break;
        }
        case TOK_START_TAG: {
          enum node_type_t type = tagname_to_node_type(t.tag_name);
          struct node_t* push = create_node(type);
          push->tag_name = t.tag_name;

          node_add_child(ptr, push); 
          ptr = push;
          break;
        }
        case TOK_END_TAG: {
          ptr = dom_pop(ptr);
          break;
        }
        default: {break;}
      }
    }

    print_tree(root, 0);

  } else {
    printf("HTML BODY: %s\n", body); 
  }

  close(sock);

  return 0;
}
