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

void zero_memory(struct Memory *memory) {}

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

static void u1x1m_set(uint8_t item, uint8_t items[125000], int index) {
  uint8_t old_byte = items[index / 8];

  const uint8_t one = 1;

  int byte_index = index % 8;

  uint8_t new_byte = (old_byte & ~(one << byte_index)) | (item << byte_index);

  items[index / 8] = new_byte;
}

static int parse_chars(
  uint8_t raw[1000000],
  uint8_t chars[1000000],
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
static int tokeniser_state_machine(enum tokeniser state, enum elm_char ch) {
  switch (state) {
  case tokeniser_exposing:
    switch (ch) {
    case char_equals:
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_e:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_hex;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_lower_name;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_upper_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_lower_name;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return -1;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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
      return tokeniser_start;
    case char_0:
      return tokeniser_0;
    case char_newline:
      return tokeniser_start;
    case char_close_parens:
      return tokeniser_start;
    case char_open_parens:
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

static int make_tokeniser_states(
  uint8_t chars[1000000],
  uint8_t states[1000000],
  int length) {

  enum tokeniser state = tokeniser_start;
  for (int i = 0; i < length; ++i) {
    enum elm_char ch = chars[i];
    const int result = tokeniser_state_machine(state, ch);
    if (result < 0) {
      return result;
    }
    states[i] = result;
  }

  return 0;
}

static void make_no_tokens(
  uint8_t state[1000000],
  uint8_t no_tokens[125000],
  int length) {

  for (int i = 0; i < length; ++i) {
    enum tokeniser current_state = state[i];
    u1x1m_set(current_state != tokeniser_start, no_tokens, i);
  }
}

static void make_is_compound_token(
  uint8_t state[1000000],
  uint8_t one_token[125000],
  int length) {

  enum tokeniser previous_state = tokeniser_start;
  for (int i = 0; i < length; ++i) {
    enum tokeniser current_state = state[i];
    u1x1m_set(
          current_state == tokeniser_start
            && previous_state != tokeniser_start,
          one_token,
          i);
    previous_state = current_state;
  }
}

static void make_is_simple_token(
  uint8_t state[1000000],
  uint8_t is_simple_token[125000],
  int length) {

  for (int i = 0; i < length; ++i) {
    const enum tokeniser current_state = state[i];
    u1x1m_set(current_state == tokeniser_start, is_simple_token, i);
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
    const int result = parse_chars(
      memory->raw,
      memory->chars,
      memory->raw_length);
    if (result != 0) {
      return result;
    }
  }

  {
    const int result =
      make_tokeniser_states(
        memory->chars,
        memory->tokeniser_state,
        memory->raw_length);
    if (result != 0) {
      return result;
    }
  }

  make_no_tokens(
    memory->tokeniser_state,
    memory->is_no_tokens,
    memory->raw_length);

  make_is_compound_token(
    memory->tokeniser_state,
    memory->is_compound_token,
    memory->raw_length);

  make_is_simple_token(
    memory->tokeniser_state,
    memory->is_simple_token,
    memory->raw_length);

  return 0;
}
