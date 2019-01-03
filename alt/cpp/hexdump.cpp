// license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

#include <iostream>
#include <fstream>
#include <memory>

const int CHUNK_SIZE {16};
const int MAX_BYTE {256};

typedef struct HEXA_BYTE_ {
  uint8_t c[2];
} HEXA_BYTE;

int main(int argc, char *argv[]) {

  const char hexadecimal[16] {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};

  HEXA_BYTE hexadecimalByte[MAX_BYTE];
  for(int index = 0; index < MAX_BYTE; ++index) {
    uint8_t *c = hexadecimalByte[index].c;
    c[0] = hexadecimal[(index >> 4) & 0xF];
    c[1] = hexadecimal[index & 0xF];
  }

  char asciiFilter[MAX_BYTE];
  for(int index = 0; index < MAX_BYTE; ++index) {
    asciiFilter[index] = index >= 0x20 && index <= 0x7e ? (char)index : '.';
  }

  std::unique_ptr<std::istream> is;
  if(argc > 1) {
    is = std::make_unique<std::ifstream>(argv[1], std::ios::binary);
  }
  
  std::istream &in{is ? *is : std::cin};

  char chunk[CHUNK_SIZE];
  char outputBuffer[1024];

  long offset = 0;
  while(true) {
    // encode offset
    const int endOffset {16};
    int outIndex {endOffset};

    // encode LSB 3 bytes
    const int nibbleMask {0xFF};
    *((HEXA_BYTE *)&outputBuffer[endOffset - 2]) = hexadecimalByte[offset & nibbleMask];
    *((HEXA_BYTE *)&outputBuffer[endOffset - 4]) = hexadecimalByte[(offset >> 8) & nibbleMask];
    *((HEXA_BYTE *)&outputBuffer[endOffset - 6]) = hexadecimalByte[(offset >> 16) & nibbleMask];

    // encode remainder - stop encoding when remainder is 0 and remember startIndex
    int startIndex {endOffset - 6};
    long offsetEncode {(offset >> 24) & 0xFFFFFFFFFFL};
    for(int index {0}; offsetEncode != 0; ++index ) {
       outputBuffer[--startIndex] = hexadecimal[(int)(offsetEncode & 0xF)];
       offsetEncode = offsetEncode >> 4;
    }

    in.read(chunk, CHUNK_SIZE);
    int length = in.gcount();
    if(length == 0 && !in) {
      outputBuffer[outIndex++] = '\n';
      std::cout.write(outputBuffer + startIndex, outIndex - startIndex);
      break;
    }
    
    outputBuffer[outIndex++] = ' ';


    if((CHUNK_SIZE == 16) && (length == CHUNK_SIZE)) {
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*0]) = hexadecimalByte[(uint8_t)chunk[0]];
       outputBuffer[outIndex+3*0+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*1]) = hexadecimalByte[(uint8_t)chunk[1]];
       outputBuffer[outIndex+3*1+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*2]) = hexadecimalByte[(uint8_t)chunk[2]];
       outputBuffer[outIndex+3*2+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*3]) = hexadecimalByte[(uint8_t)chunk[3]];
       outputBuffer[outIndex+3*3+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*4]) = hexadecimalByte[(uint8_t)chunk[4]];
       outputBuffer[outIndex+3*4+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*5]) = hexadecimalByte[(uint8_t)chunk[5]];
       outputBuffer[outIndex+3*5+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*6]) = hexadecimalByte[(uint8_t)chunk[6]];
       outputBuffer[outIndex+3*6+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*7]) = hexadecimalByte[(uint8_t)chunk[7]];
       outputBuffer[outIndex+3*7+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*8]) = hexadecimalByte[(uint8_t)chunk[8]];
       outputBuffer[outIndex+3*8+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*9]) = hexadecimalByte[(uint8_t)chunk[9]];
       outputBuffer[outIndex+3*9+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*10]) = hexadecimalByte[(uint8_t)chunk[10]];
       outputBuffer[outIndex+3*10+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*11]) = hexadecimalByte[(uint8_t)chunk[11]];
       outputBuffer[outIndex+3*11+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*12]) = hexadecimalByte[(uint8_t)chunk[12]];
       outputBuffer[outIndex+3*12+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*13]) = hexadecimalByte[(uint8_t)chunk[13]];
       outputBuffer[outIndex+3*13+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*14]) = hexadecimalByte[(uint8_t)chunk[14]];
       outputBuffer[outIndex+3*14+2] = ' ';
       *((HEXA_BYTE *)&outputBuffer[outIndex+3*15]) = hexadecimalByte[(uint8_t)chunk[15]];
       outputBuffer[outIndex+3*15+2] = ' ';
       outIndex = outIndex + CHUNK_SIZE * 3;
    }
    else {
      // encode chunk
      for(int index = 0; index < length; ++index) {
        *((HEXA_BYTE *)&outputBuffer[outIndex]) = hexadecimalByte[(uint8_t)chunk[index]];
        outIndex = outIndex + 2;
        outputBuffer[outIndex++] = ' ';
      } 
      // pad last chunk
      if(length < CHUNK_SIZE) {
        int padBytes = (CHUNK_SIZE - length) * 3;
        for(int index = 0; index < padBytes; ++index) {
          outputBuffer[outIndex++] = ' ';
        }
      }
    }

    // output ascii part
    outputBuffer[outIndex++] = ' ';
    outputBuffer[outIndex++] = '>';
    for(int index = 0; index < length; ++index) {
      outputBuffer[outIndex++] = asciiFilter[(uint8_t)chunk[index]];
    }
    outputBuffer[outIndex++] = '<';

    outputBuffer[outIndex++] = '\n';
    std::cout.write(outputBuffer + startIndex, outIndex - startIndex);
    offset = offset + length;
  }

}


