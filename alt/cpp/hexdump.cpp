// license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

#include <iostream>
#include <fstream>
#include <memory>

const int CHUNK_SIZE{16};
const int MAX_BYTE{256};

int main(int argc, char *argv[]) {

  char hexadecimal[16] {'0','1','2','3','4','5','6','7','8','9', 'a','b','c','d','e','f' };

  char hexadecimalByte[MAX_BYTE*2];
  for(int index = 0; index < MAX_BYTE; ++index) {
    hexadecimalByte[index * 2 + 1] = hexadecimal[index & 0xF];
    hexadecimalByte[index * 2] = hexadecimal[(index >> 4) & 0xF];
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
    int outIndex = 0;
    long offsetEncode = offset;
    for(int index = 0; index < 6 || offsetEncode != 0; ++index ) {
       outputBuffer[outIndex++] = hexadecimal[(int)offsetEncode & 0xF];
       offsetEncode = offsetEncode >> 4;
    }
    // reverse
    int middle = outIndex/2;
    for(int index = 0; index < middle; ++index){
       int index2 = outIndex - index - 1;
       char val = outputBuffer[index];
       outputBuffer[index] = outputBuffer[index2];
       outputBuffer[index2] = val;
    }


    in.read(chunk, CHUNK_SIZE);
    int length = in.gcount();
    if(length == 0 && !in) {
      outputBuffer[outIndex++] = '\n';
      std::cout.write(outputBuffer, outIndex);
      break;
    }
    
    // encode chunk
    outputBuffer[outIndex++] = ' ';
    for(int index = 0; index < length; ++index) {
      int hexIndex = (chunk[index] & 0xFF) << 1;
      outputBuffer[outIndex++] = hexadecimalByte[hexIndex++];
      outputBuffer[outIndex++] = hexadecimalByte[hexIndex];
      outputBuffer[outIndex++] = ' ';
    }
    // pad last chunk
    if(length < CHUNK_SIZE) {
      int padBytes = (CHUNK_SIZE - length) * 3;
      for(int index = 0; index < padBytes; ++index) {
        outputBuffer[outIndex++] = ' ';
      }
    }

    outputBuffer[outIndex++] = ' ';
    outputBuffer[outIndex++] = '>';
    for(int index = 0; index < length; ++index) {
      outputBuffer[outIndex++] = asciiFilter[chunk[index] & 0xFF];
    }
    outputBuffer[outIndex++] = '<';

    outputBuffer[outIndex++] = '\n';
    std::cout.write(outputBuffer, outIndex);
    offset = offset + length;

  }

}


