// license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

#include <iostream>
#include <fstream>
#include <memory>

const int CHUNK_SIZE {16};
const int MAX_BYTE {256};
typedef struct HEXA_BYTE_ {
  uint8_t c[2];
} HEXA_BYTE;
static const char hexadecimal[16] {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};
static HEXA_BYTE hexadecimalByte[MAX_BYTE];
static char asciiFilter[MAX_BYTE];

void init_tables() {
  for(int index = 0; index < MAX_BYTE; ++index) {
    uint8_t *c = hexadecimalByte[index].c;
    c[0] = hexadecimal[(index >> 4) & 0xF];
    c[1] = hexadecimal[index & 0xF];
  }

  for(int index = 0; index < MAX_BYTE; ++index) {
    asciiFilter[index] = index >= 0x20 && index <= 0x7e ? (char)index : '.';
  }
}

void transform(long offset, int length, const char *chunk, char *buffer, int *start, int *end) {

  // encode offset
  const int endOffset {16};
  int outIndex {endOffset};

  // encode LSB 3 bytes
  const int nibbleMask {0xFF};
  *((HEXA_BYTE *)&buffer[endOffset - 2]) = hexadecimalByte[offset & nibbleMask];
  *((HEXA_BYTE *)&buffer[endOffset - 4]) = hexadecimalByte[(offset >> 8) & nibbleMask];
  *((HEXA_BYTE *)&buffer[endOffset - 6]) = hexadecimalByte[(offset >> 16) & nibbleMask];

  // encode remainder - stop encoding when remainder is 0 and remember startIndex
  int startIndex {endOffset - 6};
  long offsetEncode {(offset >> 24) & 0xFFFFFFFFFFL};
  for(int index {0}; offsetEncode != 0; ++index ) {
    buffer[--startIndex] = hexadecimal[(int)(offsetEncode & 0xF)];
    offsetEncode = offsetEncode >> 4;
  }

  if(length > 0) {
    buffer[outIndex++] = ' ';

    if((CHUNK_SIZE == 16) && (length == CHUNK_SIZE)) {
       *((HEXA_BYTE *)&buffer[outIndex+3*0]) = hexadecimalByte[(uint8_t)chunk[0]];
       buffer[outIndex+3*0+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*1]) = hexadecimalByte[(uint8_t)chunk[1]];
       buffer[outIndex+3*1+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*2]) = hexadecimalByte[(uint8_t)chunk[2]];
       buffer[outIndex+3*2+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*3]) = hexadecimalByte[(uint8_t)chunk[3]];
       buffer[outIndex+3*3+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*4]) = hexadecimalByte[(uint8_t)chunk[4]];
       buffer[outIndex+3*4+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*5]) = hexadecimalByte[(uint8_t)chunk[5]];
       buffer[outIndex+3*5+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*6]) = hexadecimalByte[(uint8_t)chunk[6]];
       buffer[outIndex+3*6+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*7]) = hexadecimalByte[(uint8_t)chunk[7]];
       buffer[outIndex+3*7+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*8]) = hexadecimalByte[(uint8_t)chunk[8]];
       buffer[outIndex+3*8+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*9]) = hexadecimalByte[(uint8_t)chunk[9]];
       buffer[outIndex+3*9+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*10]) = hexadecimalByte[(uint8_t)chunk[10]];
       buffer[outIndex+3*10+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*11]) = hexadecimalByte[(uint8_t)chunk[11]];
       buffer[outIndex+3*11+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*12]) = hexadecimalByte[(uint8_t)chunk[12]];
       buffer[outIndex+3*12+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*13]) = hexadecimalByte[(uint8_t)chunk[13]];
       buffer[outIndex+3*13+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*14]) = hexadecimalByte[(uint8_t)chunk[14]];
       buffer[outIndex+3*14+2] = ' ';
       *((HEXA_BYTE *)&buffer[outIndex+3*15]) = hexadecimalByte[(uint8_t)chunk[15]];
       buffer[outIndex+3*15+2] = ' ';
       outIndex = outIndex + CHUNK_SIZE * 3;
    }
    else {
      // encode chunk
      for(int index = 0; index < length; ++index) {
        *((HEXA_BYTE *)&buffer[outIndex]) = hexadecimalByte[(uint8_t)chunk[index]];
        outIndex = outIndex + 2;
        buffer[outIndex++] = ' ';
      } 
      // pad last chunk
      if(length < CHUNK_SIZE) {
        int padBytes = (CHUNK_SIZE - length) * 3;
        for(int index = 0; index < padBytes; ++index) {
          buffer[outIndex++] = ' ';
        }
      }
    }

    // output ascii part
    buffer[outIndex++] = ' ';
    buffer[outIndex++] = '>';
    for(int index = 0; index < length; ++index) {
      buffer[outIndex++] = asciiFilter[(uint8_t)chunk[index]];
    }
    buffer[outIndex++] = '<';
  }

  buffer[outIndex++] = '\n';

  *start = startIndex;
  *end = outIndex;
}

int main(int argc, char *argv[]) {

  init_tables();

  std::unique_ptr<std::istream> is;
  if(argc > 1) {
    is = std::make_unique<std::ifstream>(argv[1], std::ios::binary);
  }
  
  std::istream &in{is ? *is : std::cin};

  char chunk[CHUNK_SIZE];
  char buffer[1024];

  long offset = 0;
  bool last_chunk = false;
  while(!last_chunk) {

    in.read(chunk, CHUNK_SIZE);
    int length = in.gcount();
    last_chunk = (length == 0 && !in);

    int startIndex;
    int outIndex;
    transform(offset, length, chunk, buffer, &startIndex, &outIndex);

    std::cout.write(buffer + startIndex, outIndex - startIndex);
    offset = offset + length;
  }

}


