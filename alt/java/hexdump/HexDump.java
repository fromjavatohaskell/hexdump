package hexdump;

// license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.IOException;

public class HexDump {

	private static void hexdump(String filename) {

		final int CHUNK_SIZE = 16;
		long offset = 0;
		byte[] chunk = new byte[CHUNK_SIZE];
		// buffer one line
		byte[] outputBuffer = new byte[1024];
		
		byte[] hexadecimal = {'0','1','2','3','4','5','6','7','8','9',
			'a','b','c','d','e','f'
		};
		
		final int MAX_BYTE = 256;
		byte[] hexadecimalByte = new byte[MAX_BYTE*2];
		for(int index = 0; index < MAX_BYTE; ++index) {
			hexadecimalByte[index * 2 + 1] = hexadecimal[index & 0xF];
			hexadecimalByte[index * 2] = hexadecimal[(index >>> 4) & 0xF];
		}
		
		byte[] asciiFilter = new byte[MAX_BYTE];
		for(int index = 0; index < MAX_BYTE; ++index) {
			asciiFilter[index] = index >= 0x20 && index <= 0x7e ? (byte)index : (byte)'.';
		}
		
		try (BufferedInputStream reader = new BufferedInputStream(filename != null ?
				new FileInputStream(filename) : System.in);
				BufferedOutputStream writer = new BufferedOutputStream(System.out)) {
			while(true) {
				int outIndex = 0;
				int length = reader.read(chunk);
				// encode offset
				long offsetEncode = offset;
				for(int index = 0; index < 6 || offsetEncode != 0; ++index ) {
					outputBuffer[outIndex++] = hexadecimal[(int)offsetEncode & 0xF];
					offsetEncode = offsetEncode >>> 4;
				}
				// reverse
				for(int index = 0; index < outIndex/2; ++index){
					int index2 = outIndex - index - 1;
					byte val = outputBuffer[index];
					outputBuffer[index] = outputBuffer[index2];
					outputBuffer[index2] = val;					
				}
				
				if(length < 0) {
					outputBuffer[outIndex++] = '\n';
					System.out.write(outputBuffer, 0, outIndex);
					return;
				}
				// encode chunk
				outputBuffer[outIndex++] = ' ';
				for(int index = 0; index < length; ++index) {
					int hexIndex = (chunk[index] & 0xFF) << 1; 
					outputBuffer[outIndex++] = hexadecimalByte[hexIndex++];
					outputBuffer[outIndex++] = hexadecimalByte[hexIndex];
					outputBuffer[outIndex++] = ' ';
				}
				if(length < CHUNK_SIZE) {
					// pad hex
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
				System.out.write(outputBuffer, 0, outIndex);
				offset = offset + length;
			}
		}
		catch (IOException ex) {
			ex.printStackTrace();
		}
	}
	
	public static void main(String[] args) {
		hexdump(args.length > 0 ? args[0] : null);
	}
}
