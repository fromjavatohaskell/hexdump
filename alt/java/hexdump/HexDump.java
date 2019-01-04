package hexdump;

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
				int length = reader.read(chunk);
				// encode offset
				long offsetEncode = offset;
				int startIndex = 16;
				int outIndex = startIndex;
				
				{
					int index = (int)(offsetEncode << 1) & 0x1FE;
					outputBuffer[--startIndex] = hexadecimalByte[index+1];
					outputBuffer[--startIndex] = hexadecimalByte[index];
				}
				{
					int index = (int)(offsetEncode >> 7) & 0x1FE;
					outputBuffer[--startIndex] = hexadecimalByte[index+1];
					outputBuffer[--startIndex] = hexadecimalByte[index];
				}
				{
					int index = (int)(offsetEncode >> 15) & 0x1FE;
					outputBuffer[--startIndex] = hexadecimalByte[index+1];
					outputBuffer[--startIndex] = hexadecimalByte[index];
				}
			    // encode remainder - stop encoding when remainder is 0 and remember startIndex
				offsetEncode = (offsetEncode >> 24) & 0xFFFFFFFFFFL;
			    for(; offsetEncode != 0; ) {
			       outputBuffer[--startIndex] = hexadecimal[(int)(offsetEncode & 0xF)];
			       offsetEncode = offsetEncode >> 4;
			    }				
				
				if(length < 0) {
					outputBuffer[outIndex++] = '\n';
					System.out.write(outputBuffer, startIndex, outIndex - startIndex);
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
				System.out.write(outputBuffer, startIndex, outIndex - startIndex);
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
