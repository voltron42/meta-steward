package meta_steward.java;

import java.io.ByteArrayOutputStream;

public class BetterByteArrayOutputStream extends ByteArrayOutputStream {
  public byte[] getBuffer() {
    return buf;
  }
}