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

static int read_raw(FILE *input_file, struct u8x1m *buffer) {
  buffer->_length = fread(buffer->_items, 1, 1000000, input_file);

  int result = ferror(input_file);
  if (result != 0) {
    return result;
  }

  result = feof(input_file);
  if (result == 0) {
    return -1;
  }

  return 0;
}

static int u8x1m_get(int index, struct u8x1m raw) {
  if (index >= raw._length || index < 0) {
    return -1;
  }

  return raw._items[index];
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

static int u1x1m_append(uint8_t item, struct u1x1m *items) {
  if (items->_length == 1000000) {
    return -1;
  }

  uint8_t old_byte = items->_items[(items->_length) / 8];

  const uint8_t one = 1;

  int byte_index = (items->_length) % 8;

  uint8_t new_byte = (old_byte & ~(one << byte_index)) | (item << byte_index);

  items->_items[(items->_length) / 8] = new_byte;
  ++(items->_length);

  return 0;
}

static int u8x1m_append(uint8_t item, struct u8x1m *items) {
  if (items->_length == 1000000) {
    return -1;
  }

  items->_items[items->_length] = item;
  ++items->_length;

  return 0;
}

static int parse_chars(struct u8x1m raw, struct u8x1m *chars) {
  for (int i = 0;; ++i) {
    int result = u8x1m_get(i, raw);
    if (result < 0) {
      return 0;
    }

    result = parse_char(result);
    if (result < 0) {
      return result;
    }

    result = u8x1m_append(result, chars);
    if (result != 0) {
      return result;
    }
  }
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

static int make_tokeniser_states(struct u8x1m chars, struct u8x1m *states) {
  enum tokeniser state = tokeniser_start;
  for (int i = 0;; ++i) {
    int result = u8x1m_get(i, chars);
    if (result != 0) {
      break;
    }

    enum elm_char ch = result;
    result = tokeniser_state_machine(state, ch);
    if (result < 0) {
      return result;
    }
    result = u8x1m_append(result, states);
    if (result != 0) {
      return result;
    }
  }

  return 0;
}

static int make_no_tokens(struct u8x1m state, struct u1x1m *no_tokens) {
  for (int i = 0;; ++i) {
    int result = u8x1m_get(i, state);
    if (result != 0) {
      break;
    }
    enum tokeniser current_state = result;
    result = u1x1m_append(current_state != tokeniser_start, no_tokens);
    if (result != 0) {
      return result;
    }
  }
  return 0;
}

static int make_one_tokens(struct u8x1m state, struct u1x1m *one_token) {
  enum tokeniser previous_state = tokeniser_start;
  for (int i = 0;; ++i) {
    int result = u8x1m_get(i, state);
    if (result != 0) {
      break;
    }
    enum tokeniser current_state = result;
    result = u1x1m_append(current_state == tokeniser_start &&
                              previous_state == tokeniser_start,
                          one_token);
    if (result != 0) {
      return result;
    }

    previous_state = current_state;
  }

  return 0;
}

int format(FILE *input_file, FILE *output_file, struct Memory *memory) {
  int result = read_raw(input_file, &memory->raw);
  if (result != 0) {
    return result;
  }

  result = parse_chars(memory->raw, &memory->chars);
  if (result != 0) {
    return result;
  }

  result = make_tokeniser_states(memory->chars, &memory->tokeniser_state);
  if (result != 0) {
    return result;
  }

  result = make_no_tokens(memory->tokeniser_state, &memory->no_tokens);
  if (result != 0) {
    return result;
  }

  return make_one_tokens(memory->tokeniser_state, &memory->one_token);
}
