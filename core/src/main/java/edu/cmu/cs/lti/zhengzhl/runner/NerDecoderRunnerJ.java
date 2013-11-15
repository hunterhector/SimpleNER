package edu.cmu.cs.lti.zhengzhl.runner;

import edu.cmu.cs.lti.zhengzhl.algorithm.BioNerDecoder;
import edu.cmu.cs.lti.zhengzhl.algorithm.StandardNerDecoder;
import edu.cmu.cs.lti.zhengzhl.algorithm.ViterbiDecoder;
import edu.cmu.cs.lti.zhengzhl.io.Gazetteer;
import edu.cmu.cs.lti.zhengzhl.io.TokenPerLineReader;
import edu.cmu.cs.lti.zhengzhl.model.Token;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/14/13
 * Time: 2:08 PM
 */
public class NerDecoderRunnerJ {

    public static void main(String[] args) throws IOException {
        long start = System.nanoTime();

        System.out.println("Current working directory " + System.getProperty("user.dir"));

        String modelPath = args[0];
        String dataPath = args[1];
        String gazePath = args[2];
        String outputPath = args[3];

        FileWriter out = new FileWriter(outputPath);

        System.out.println("Reading tokens");
        TokenPerLineReader reader = new TokenPerLineReader(new File(dataPath));
        System.out.println("Reading gazetteer");
        Gazetteer gaze = new Gazetteer(new File(gazePath));
        System.out.println("Reading weights");

        Map<String, Double> weights = new HashMap<String, Double>();

        BufferedReader br = new BufferedReader(new FileReader(new File(modelPath)));
        String line;
        while ((line = br.readLine()) != null) {
            String[] parts = line.split(" ");
            weights.put(parts[0], Double.parseDouble(parts[1]));
        }
        String[] tagNames = reader.getTags();


        System.out.println("Preparing decode");
        ViterbiDecoder decoder = new StandardNerDecoder(tagNames, "<START>", weights,gaze);

        System.out.println("Decoding sentences ");

        int counter = 0;
        while (reader.hasNext()) {
            Token[] sentence = reader.nextSentence(true);

            long decodeStart = System.nanoTime();
            String[] results = decoder.decode(sentence);
            System.out.println("Decode time " + (System.nanoTime() - decodeStart) / 1e9);

            //ignore sentence end symbol
            for (int i = 0; i < results.length - 1; i++) {
                out.write(sentence[i].toString() + " " + results[i].toString() + "\n");
            }

            counter += 1;
            if (counter % 100 == 0) {
                System.out.print(".");
            }
        }

        System.out.println();
        out.close();
        System.out.println("Finished in: " + (System.nanoTime() - start) / 1e9 + "s");


    }

}
