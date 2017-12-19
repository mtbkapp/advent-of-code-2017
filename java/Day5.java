import java.io.*;

public class Day5 {
  private static final int SIZE = 1097;
  private static final String filename = "../resources/day5.txt";

  static int[] readInput() throws IOException {
    int[] output = new int[SIZE]; 
    BufferedReader rdr = null;
    String line;
    int i = 0;
    try {
      rdr = new BufferedReader(new FileReader(filename));
      while((line = rdr.readLine()) != null) {
        output[i] = Integer.parseInt(line);
        i++;
      }
    } finally {
      if (rdr != null) {
        rdr.close();
      }
    }

    return output;
  }

  static int solvePart2(int[] program) {
    int jumpCount = 0;
    int i = 0;
    while(i < SIZE) {
      int offset = program[i];
      if (offset >= 3) {
        program[i] = offset - 1;
      } else {
        program[i] = offset + 1;
      }

      i = i + offset;
      jumpCount++;
    }

    return jumpCount;
  }

  public static void main(String[] args) throws IOException {
    System.out.println(solvePart2(readInput()));
  }
}
