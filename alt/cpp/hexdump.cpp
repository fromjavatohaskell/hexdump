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

  char hexadecimal[16] {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};

  HEXA_BYTE hexadecimalByte[MAX_BYTE];
  for(int index = 0; index < MAX_BYTE; ++index) {
    HEXA_BYTE *hexa_byte = &hexadecimalByte[index];
    hexa_byte->c[0] = hexadecimal[(index >> 4) & 0xF];
    hexa_byte->c[1] = hexadecimal[index & 0xF];
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


