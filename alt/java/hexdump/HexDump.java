package hexdump;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.IOException;

public class HexDump {

	static final int CHUNK_SIZE = 16;
	static final byte[] hexadecimal = {'0','1','2','3','4','5','6','7','8','9', 'a','b','c','d','e','f'};
	static final int MAX_BYTE = 256;
	static final byte[] hexadecimalByte = new byte[MAX_BYTE*2];
	static final byte[] asciiFilter = new byte[MAX_BYTE];
	static {
		for(int index = 0; index < MAX_BYTE; ++index) {
			hexadecimalByte[index * 2 + 1] = hexadecimal[index & 0xF];
			hexadecimalByte[index * 2] = hexadecimal[(index >>> 4) & 0xF];
		}

		for(int index = 0; index < MAX_BYTE; ++index) {
			asciiFilter[index] = index >= 0x20 && index <= 0x7e ? (byte)index : (byte)'.';
		}
	}

	private static class  Offset {
		int start;
		int end;
	}
	
	private static void transform(long offset, int length, byte []chunk, byte []buffer, Offset bufferOffset) {
		// encode offset
		long offsetEncode = offset;
		int startIndex = 16;
		int outIndex = startIndex;
		
		{
			int index = (int)(offsetEncode << 1) & 0x1FE;
			buffer[--startIndex] = hexadecimalByte[index+1];
			buffer[--startIndex] = hexadecimalByte[index];
		}
		{
			int index = (int)(offsetEncode >> 7) & 0x1FE;
			buffer[--startIndex] = hexadecimalByte[index+1];
			buffer[--startIndex] = hexadecimalByte[index];
		}
		{
			int index = (int)(offsetEncode >> 15) & 0x1FE;
			buffer[--startIndex] = hexadecimalByte[index+1];
			buffer[--startIndex] = hexadecimalByte[index];
		}
	    // encode remainder - stop encoding when remainder is 0 and remember startIndex
		offsetEncode = (offsetEncode >> 24) & 0xFFFFFFFFFFL;
	    for(; offsetEncode != 0; ) {
	       buffer[--startIndex] = hexadecimal[(int)(offsetEncode & 0xF)];
	       offsetEncode = offsetEncode >> 4;
	    }				
		if(length > 0) {
			// encode chunk
			buffer[outIndex++] = ' ';
			for(int index = 0; index < length; ++index) {
				int hexIndex = (chunk[index] & 0xFF) << 1; 
				buffer[outIndex++] = hexadecimalByte[hexIndex++];
				buffer[outIndex++] = hexadecimalByte[hexIndex];
				buffer[outIndex++] = ' ';
			}
			if(length < CHUNK_SIZE) {
				// pad hex
				int padBytes = (CHUNK_SIZE - length) * 3;
				for(int index = 0; index < padBytes; ++index) {
					buffer[outIndex++] = ' ';
				}
			}
			buffer[outIndex++] = ' ';
			buffer[outIndex++] = '>';
			for(int index = 0; index < length; ++index) {
				buffer[outIndex++] = asciiFilter[chunk[index] & 0xFF];
			}
			buffer[outIndex++] = '<';
		}
		buffer[outIndex++] = '\n';
		bufferOffset.start = startIndex;
		bufferOffset.end = outIndex;
	}
	
	
	private static void hexdump(String filename) {

		long offset = 0;
		byte[] chunk = new byte[CHUNK_SIZE];
		// buffer one line
		byte[] outputBuffer = new byte[1024];
		Offset outputOffset = new Offset();
		
		try (BufferedInputStream reader = new BufferedInputStream(filename != null ?
				new FileInputStream(filename) : System.in);
				BufferedOutputStream writer = new BufferedOutputStream(System.out)) {
			
			boolean lastChunk = false;
			while(!lastChunk) {
				int length = reader.read(chunk);
				lastChunk = length < 0;
				transform(offset, length, chunk, outputBuffer, outputOffset);
				System.out.write(outputBuffer, outputOffset.start, outputOffset.end - outputOffset.start);
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
