#include "trumat.h"

enum elm_char {
  char_0,
  char_equals,
  char_newline,
  char_close_parens,
  char_open_parens,
  char_g,
  char_n,
  char_i,
  char_s,
  char_o,
  char_p,
  char_x,
  char_X,
  char_space,
  char_e,
  char_l,
  char_u,
  char_d,
  char_m,
};

enum token {
  token_upper_name,
  token_in,
  token_number,
  token_else,
  token_module,
  token_lower_name,
  token_space,
  token_open_parens,
  token_close_parens,
  token_newline,
  token_equals,
  token_exposing,
  token_compound_char,
};

void zero_memory(struct Memory *memory) { memory->raw_length = 0; }

static int read_raw(FILE *input_file, uint8_t buffer[1000000]) {
  int length = fread(buffer, 1, 1000000, input_file);

  int result = ferror(input_file);
  if (result != 0) {
    return -1;
  }

  result = feof(input_file);
  if (result == 0) {
    return -1;
  }

  return length;
}

static int parse_char(uint8_t raw) {
  switch (raw) {
  case 'm':
    return char_m;
  case 'd':
    return char_d;
  case 'u':
    return char_u;
  case 'l':
    return char_l;
  case 'e':
    return char_e;
  case ' ':
    return char_space;
  case 'X':
    return char_X;
  case 'x':
    return char_x;
  case 'p':
    return char_p;
  case 'o':
    return char_o;
  case 's':
    return char_s;
  case 'i':
    return char_i;
  case 'n':
    return char_n;
  case 'g':
    return char_g;
  case '(':
    return char_open_parens;
  case ')':
    return char_close_parens;
  case '\n':
    return char_newline;
  case '=':
    return char_equals;
  case '0':
    return char_0;
  }
  return -1;
}

static int parse_chars(uint8_t raw[1000000], uint8_t chars[1000000],
                       int length) {

  for (int i = 0; i < length; ++i) {
    int result = parse_char(raw[i]);
    if (result < 0) {
      return result;
    }

    chars[i] = result;
  }
  return 0;
}

enum tokeniser {
  tokeniser_start,
  tokeniser_m,
  tokeniser_l,
  tokeniser_e,
  tokeniser_upper_name,
  tokeniser_p,
  tokeniser_o,
  tokeniser_i,
  tokeniser_lower_name,
  tokeniser_0,
  tokeniser_0x,
  tokeniser_im,
  tokeniser_in,
  tokeniser_po,
  tokeniser_mo,
  tokeniser_le,
  tokeniser_ex,
  tokeniser_el,
  tokeniser_hex,
  tokeniser_imp,
  tokeniser_mod,
  tokeniser_exp,
  tokeniser_els,
  tokeniser_impo,
  tokeniser_modu,
  tokeniser_expo,
  tokeniser_else,
  tokeniser_modul,
  tokeniser_expos,
  tokeniser_module,
  tokeniser_exposi,
  tokeniser_exposin,
  tokeniser_exposing,
};

// reserved keywords in Elm: as case else exposing if import in let module
// of port then type where
static int tokeniser_state_machine(enum tokeniser state, enum elm_char ch,
                                   uint8_t tokens[1000000], int i) {

  switch (state) {
  case tokeniser_exposing:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_exposing;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_exposing;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_exposing;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_exposing;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_exposing;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_exposin:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_exposing;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_exposi:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_exposin;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_module:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_module;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_module;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_module;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_module;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_module;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_expos:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_exposi;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_modul:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_module;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_else:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_else;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_else;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_else;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_else;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_else;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_expo:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_expos;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_modu:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_modul;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_impo:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_els:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_else;
      return tokeniser_else;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_exp:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_expo;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_mod:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_modu;
    };
  case tokeniser_imp:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_impo;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_hex:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_number;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_hex;
    case char_newline:
      tokens[i - 1] = token_number;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_number;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_number;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return -1;
    case char_n:
      return -1;
    case char_i:
      return -1;
    case char_s:
      return -1;
    case char_o:
      return -1;
    case char_p:
      return -1;
    case char_x:
      return -1;
    case char_X:
      return -1;
    case char_space:
      tokens[i - 1] = token_number;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_hex;
    case char_l:
      return -1;
    case char_m:
      return -1;
    case char_d:
      return tokeniser_hex;
    case char_u:
      return -1;
    };
  case tokeniser_el:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_els;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_ex:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_exp;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_le:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_mo:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_mod;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_po:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_in:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_in;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_in;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_in;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_in;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_in;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_im:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_lower_name;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_imp;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_0x:
    switch (ch) {
    case char_equals:
      return -1;
    case char_0:
      return tokeniser_hex;
    case char_newline:
      return -1;
    case char_close_parens:
      return -1;
    case char_open_parens:
      return -1;
    case char_g:
      return -1;
    case char_n:
      return -1;
    case char_i:
      return -1;
    case char_s:
      return -1;
    case char_o:
      return -1;
    case char_p:
      return -1;
    case char_x:
      return -1;
    case char_X:
      return -1;
    case char_space:
      return -1;
    case char_e:
      return tokeniser_hex;
    case char_l:
      return -1;
    case char_m:
      return -1;
    case char_d:
      return tokeniser_hex;
    case char_u:
      return -1;
    };
  case tokeniser_upper_name:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_upper_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_upper_name;
    case char_newline:
      tokens[i - 1] = token_upper_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_upper_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_upper_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_upper_name;
    case char_n:
      return tokeniser_upper_name;
    case char_i:
      return tokeniser_upper_name;
    case char_s:
      return tokeniser_upper_name;
    case char_o:
      return tokeniser_upper_name;
    case char_p:
      return tokeniser_upper_name;
    case char_x:
      return tokeniser_upper_name;
    case char_X:
      return tokeniser_upper_name;
    case char_space:
      tokens[i - 1] = token_upper_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_upper_name;
    case char_l:
      return tokeniser_el;
    case char_m:
      return tokeniser_upper_name;
    case char_d:
      return tokeniser_upper_name;
    case char_u:
      return tokeniser_upper_name;
    };
  case tokeniser_e:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_ex;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_le;
    case char_l:
      return tokeniser_el;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_l:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_le;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_m:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_mo;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_p:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_po;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_o:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_i:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_in;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_im;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_lower_name:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_lower_name;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_lower_name;
    case char_p:
      return tokeniser_lower_name;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_lower_name;
    case char_space:
      tokens[i - 1] = token_lower_name;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_lower_name;
    case char_l:
      return tokeniser_lower_name;
    case char_m:
      return tokeniser_lower_name;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  case tokeniser_0:
    switch (ch) {
    case char_equals:
      tokens[i - 1] = token_number;
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return -1;
    case char_newline:
      tokens[i - 1] = token_number;
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i - 1] = token_number;
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i - 1] = token_number;
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return -1;
    case char_n:
      return -1;
    case char_i:
      return -1;
    case char_s:
      return -1;
    case char_o:
      return -1;
    case char_p:
      return -1;
    case char_x:
      return tokeniser_0x;
    case char_X:
      return -1;
    case char_space:
      tokens[i - 1] = token_number;
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return -1;
    case char_l:
      return -1;
    case char_m:
      return -1;
    case char_d:
      return -1;
    case char_u:
      return -1;
    };
  case tokeniser_start:
    switch (ch) {
    case char_equals:
      tokens[i] = token_equals;
      return tokeniser_start;
    case char_0:
      return tokeniser_0;
    case char_newline:
      tokens[i] = token_newline;
      return tokeniser_start;
    case char_close_parens:
      tokens[i] = token_close_parens;
      return tokeniser_start;
    case char_open_parens:
      tokens[i] = token_open_parens;
      return tokeniser_start;
    case char_g:
      return tokeniser_lower_name;
    case char_n:
      return tokeniser_lower_name;
    case char_i:
      return tokeniser_i;
    case char_s:
      return tokeniser_lower_name;
    case char_o:
      return tokeniser_o;
    case char_p:
      return tokeniser_p;
    case char_x:
      return tokeniser_lower_name;
    case char_X:
      return tokeniser_upper_name;
    case char_space:
      tokens[i] = token_space;
      return tokeniser_start;
    case char_e:
      return tokeniser_e;
    case char_l:
      return tokeniser_l;
    case char_m:
      return tokeniser_m;
    case char_d:
      return tokeniser_lower_name;
    case char_u:
      return tokeniser_lower_name;
    };
  };

  return -1;
}

static void make_column_numbers(uint8_t tokens[1000000],
                                uint16_t column[1000000], int length) {
  uint16_t current_column = 0;
  for (int i = 0; i < length; ++i) {
    column[i] = current_column;

    enum token token = tokens[i];

    if (token == token_newline) {
      current_column = 0;
      continue;
    }

    if (token != token_compound_char) {
      ++current_column;
    }
  }
}

static void make_row_numbers(uint8_t chars[1000000], uint16_t row[1000000],
                             int length) {
  uint16_t current_row = 0;
  for (int i = 0; i < length; ++i) {
    row[i] = current_row;

    if (chars[i] == char_newline) {
      ++current_row;
    }
  }
}

static int tokenise(uint8_t chars[1000000], uint8_t tokens[1000000],
                    int length) {

  for (int i = 0; i < length; ++i) {
    tokens[i] = token_compound_char;
  }

  enum tokeniser state = tokeniser_start;
  for (int i = 0; i < length; ++i) {
    enum elm_char ch = chars[i];
    const int result = tokeniser_state_machine(state, ch, tokens, i);
    if (result < 0) {
      return result;
    }
    state = result;
  }

  return 0;
}

enum floor_state {
  floor_last_was_floory,
  floor_last_not_floory,
};

static int calculate_one_floor(enum token token, uint16_t column,
                               uint16_t *floor, enum floor_state *state,
                               int next_lowest_floor) {

  switch (*state) {
  case floor_last_not_floory:
    switch (token) {
    case token_upper_name:
      return 0;
    case token_equals:
      return 0;
    case token_newline:
      return 0;
    case token_close_parens:
      return 0;
    case token_open_parens:
      return 0;
    case token_space:
      return 0;
    case token_lower_name:
      return 0;
    case token_else:
      return 0;
    case token_number:
      return 0;
    case token_in:
      return 0;
    case token_exposing:
      return 0;
    case token_compound_char:
      return 0;
    case token_module:
      return 0;
    };
  case floor_last_was_floory:
    switch (token) {
    case token_upper_name:
      *floor = column;
      *state = floor_last_not_floory;
      return 0;
    case token_in:
      *floor = next_lowest_floor;
      *state = floor_last_not_floory;
      return 0;
    case token_number:
      *floor = column;
      *state = floor_last_not_floory;
      return 0;
    case token_else:
      return -1;
    case token_lower_name:
      *floor = column;
      *state = floor_last_not_floory;
      return 0;
    case token_space:
      return 0;
    case token_open_parens:
      *floor = column;
      *state = floor_last_not_floory;
      return 0;
    case token_close_parens:
      return -1;
    case token_newline:
      return 0;
    case token_equals:
      return -1;
    case token_exposing:
      return -1;
    case token_compound_char:
      return 0;
    case token_module:
      return -1;
    };
  };

  return 0;
}

static int calculate_floor(uint8_t tokens[1000000], uint16_t columns[1000000],
                           uint16_t floor[1000000], int raw_length) {

  for (int i = 0; i < raw_length; ++i) {
    floor[i] = 0;
  }

  uint16_t history_array[100];
  uint16_t *history = history_array;
  *history = 0;
  ++history;

  enum floor_state state = floor_last_not_floory;
  for (int i = 0; i < raw_length; ++i) {
    const int result = calculate_one_floor(tokens[i], columns[i], &floor[i],
                                           &state, *(history - 1));

    if (result < 0) {
      return result;
    }

    if (floor[i] > *history) {
      ++history;
      *history = floor[i];
    }

    if (floor[i] < *history) {
      --history;
    }
  }

  return 0;
}

enum depth_state {
  depth_start_top_level,
  depth_after_module,
  depth_before_top_parameter,
  depth_top_bind_expression_start,
  depth_in_parameter_pattern,
  depth_after_module_name,
  depth_after_leading_expression,
};

static int calculate_one_depth(
  enum token token,
  int is_floor,
  uint8_t previous_depth,
  uint8_t* depth,
  int* nesting,
  enum depth_state* state) {

  switch (*state) {
  case depth_after_leading_expression:
    switch (token) {
    case token_upper_name:
      if (is_floor) {
        return -1;
      }
      *state = depth_before_function_argument;
      ++*depth;
      return 0;
    case token_in:
      return -1;
    case token_number:
      if (is_floor) {
        return -1;
      }
      *state = depth_before_function_argument;
      ++*depth;
      return 0;
    case token_else:
      return -1;
    case token_lower_name:
      if (is_floor) {
         *state = depth_before_top_parameter;
         return 0;
      }
      *state = depth_after_leading_expression;
      --*depth;
      return 0;
    case token_space:
      return 0;
    case token_open_parens:
      ++*depth;
      *state = depth_start_sub_expression;
      return 0;
    case token_close_parens:
      --*nesting;
      if (*nesting == 0) {
        *state = depth_before_top_parameter;
      }
      return 0;
    case token_newline:
      return 0;
    case token_equals:
      return -1;
    case token_exposing:
      return -1;
    case token_compound_char:
      return 0;
    case token_module:
      return -1;
    };
  case depth_top_bind_expression_start:
    switch (token) {
    case token_upper_name:
      *state = depth_after_leading_expression;
      return 0;
    case token_in:
      return -1;
    case token_number:
      *state = depth_after_leading_expression;
      return 0;
    case token_else:
      return -1;
    case token_lower_name:
      *state = depth_after_leading_expression;
      return 0;
    case token_space:
      return 0;
    case token_open_parens:
      ++*nesting;
      return 0;
    case token_close_parens:
      --*nesting;
      if (*nesting == 0) {
        *state = depth_before_top_parameter;
      }
      return 0;
    case token_newline:
      return 0;
    case token_equals:
      return -1;
    case token_exposing:
      return -1;
    case token_compound_char:
      return 0;
    case token_module:
      return -1;
    };
  case depth_in_parameter_pattern:
    switch (token) {
    case token_upper_name:
      return 0;
    case token_in:
      return -1;
    case token_number:
      return 0;
    case token_else:
      return -1;
    case token_lower_name:
      return 0;
    case token_space:
      return 0;
    case token_open_parens:
      ++*nesting;
      return 0;
    case token_close_parens:
      --*nesting;
      if (*nesting == 0) {
        *state = depth_before_top_parameter;
      }
      return 0;
    case token_newline:
      return 0;
    case token_equals:
      return -1;
    case token_exposing:
      return -1;
    case token_compound_char:
      return 0;
    case token_module:
      return -1;
    };
  case depth_after_module:
    switch (token) {
    case token_upper_name:
      *state = depth_after_module_name;
      return 0;
    case token_in:
      return -1;
    case token_number:
      return -1;
    case token_else:
      return -1;
    case token_lower_name:
      return -1;
    case token_space:
      return 0;
    case token_open_parens:
      return -1;
    case token_close_parens:
      return -1;
    case token_newline:
      return 0;
    case token_equals:
      return -1;
    case token_exposing:
      return -1;
    case token_compound_char:
      return 0;
    case token_module:
      return -1;
    };
  case depth_before_top_parameter:
    switch (token) {
    case token_upper_name:
      return 0;
    case token_in:
      return -1;
    case token_number:
      return -1;
    case token_else:
      return -1;
    case token_lower_name:
      return 0;
    case token_space:
      return 0;
    case token_open_parens:
      ++*nesting;
      *state = depth_in_parameter_pattern;
      return 0;
    case token_close_parens:
      return -1;
    case token_newline:
      return 0;
    case token_equals:
      *state = depth_top_bind_expression_start;
      return 0;
    case token_exposing:
      return -1;
    case token_compound_char:
      return 0;
    case token_module:
      return -1;
    };
  case depth_start_top_level:
    switch (token) {
    case token_upper_name:
      return -1;
    case token_in:
      return -1;
    case token_number:
      return -1;
    case token_else:
      return -1;
    case token_lower_name:
      *state = depth_before_top_parameter;
      return 0;
    case token_space:
      return 0;
    case token_open_parens:
      return -1;
    case token_close_parens:
      return -1;
    case token_newline:
      return 0;
    case token_equals:
      return -1;
    case token_exposing:
      return -1;
    case token_compound_char:
      return 0;
    case token_module:
      *state = depth_after_module;
      return 0;
    };
  };

  return -1;
}

static int calculate_depth(
  uint8_t tokens[1000000],
  uint8_t is_floor[1000000],
  uint8_t depth[1000000],
  int length) {

  enum depth_state state = depth_start_top_level;

  uint16_t previous_depth = 0;

  int nesting = 0;

  for (int i = 0; i < length; ++i) {
    const int result = calculate_one_depth(
        tokens[i],
        is_floor[i],
        previous_depth,
        &depth[i],
        &nesting,
        &state);
    if (result != 0) {
      return result;
    }

    previous_depth = depth[i];
  }
  return 0;
}

static void calculate_is_floor(
    uint16_t floor[1000000],
    uint16_t column[1000000],
    uint8_t is_floor[1000000],
    int length) {

    for (int i = 0; i < length; ++i) {
        is_floor[i] = floor[i] == column[i];
    }
}

int format(FILE *input_file, FILE *output_file, struct Memory *memory) {
  {
    const int result = read_raw(input_file, memory->raw);
    if (result < 0) {
      return result;
    }
    memory->raw_length = result;
  }

  {
    const int result =
        parse_chars(memory->raw, memory->chars, memory->raw_length);
    if (result != 0) {
      return result;
    }
  }

  make_row_numbers(memory->chars, memory->row, memory->raw_length);
  make_column_numbers(memory->tokens, memory->column, memory->raw_length);

  {
    const int result =
        tokenise(memory->chars, memory->tokens, memory->raw_length);
    if (result != 0) {
      return result;
    }
  }

  {
    const int result = calculate_floor(memory->tokens, memory->column, memory->floor,
                         memory->raw_length);
    if (result != 0) {
      return result;
    }
  }

  calculate_is_floor(memory->floor, memory->column, memory->is_floor, memory->raw_length);

  return calculate_depth(
      memory->tokens,
      memory->is_floor,
      memory->depth,
      memory->raw_length);
}
