package edu.cmu.cs.lti.zhengzhl.algorithm;

import edu.cmu.cs.lti.zhengzhl.io.Gazetteer;
import edu.cmu.cs.lti.zhengzhl.model.Token;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/14/13
 * Time: 2:32 PM
 */
public class StandardNerDecoder extends ViterbiDecoder {
    Map<String, Double> weights;
    Gazetteer gaze;

    public StandardNerDecoder(String[] stateSet, String sentenceStart, Map<String, Double> weights, Gazetteer gaze) {
        super(stateSet, sentenceStart);
        this.weights = weights;
        this.gaze = gaze;
    }

    @Override
    protected double getScore(Token[] observations, int index, String previousState, String currentState) {
        double sum = 0.0;
        for (String firedFeature : getFeatures(observations, index, previousState, currentState, gaze)) {
            if (weights.containsKey(firedFeature)) {
                sum += weights.get(firedFeature);
            }
        }

        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    private List<String> getFeatures(Token[] observations, int index, String previousState, String currentState, Gazetteer gaze) {
        Token token = observations[index];
        int nToken = observations.length;
        String tagToUse = index == nToken - 1 ? "<STOP>" : currentState;
        String tagFeatureTail = String.format("Ti=%s", tagToUse);
        String text = token.text();
        List<String> allFeatures = new ArrayList<String>();

        List<String> currentFeatures = localBaseFeatures(observations,index,nToken,"i");
        List<String> surroundingFeatures = localBaseFeatures(observations,index-1,nToken,"i-1");
        surroundingFeatures.addAll(localBaseFeatures(observations,index+1,nToken,"i+1"));

        for (String cFeature : currentFeatures){
            allFeatures.add(String.format("%s:%s",cFeature,tagFeatureTail));
        }

        for (String sFeature : surroundingFeatures){
                allFeatures.add(String.format("%s:%s",sFeature,tagFeatureTail));
        }


        for (String cFeature : currentFeatures){
            for (String sFeature : surroundingFeatures) {
                allFeatures.add(String.format("%s:%s:%s",cFeature, sFeature,tagFeatureTail));
            }
        }

        String previousTagFeature = String.format("Ti-1=%s", previousState);
        allFeatures.add(previousTagFeature);

        for (String cFeature : currentFeatures){
            allFeatures.add(String.format("%s:%s:%s",cFeature, previousTagFeature,tagFeatureTail));
        }
        for (String sFeature : surroundingFeatures){
            allFeatures.add(String.format("%s:%s:%s",sFeature, previousTagFeature,tagFeatureTail));
        }

        for (int k =1;k <=4;k++){
            if (text.length() >= k){
                allFeatures.add(String.format("PREi=%s:%s", text.substring(0, k),tagFeatureTail));
            }
        }

        allFeatures.add(String.format("GAZi=%s:%s",gazeMatch(token, tagToUse, gaze),tagFeatureTail));

        allFeatures.add(String.format("CAPi=%s:%s", Character.isUpperCase(text.charAt(0)) ? "True" : "False",tagFeatureTail));

        allFeatures.add(String.format("POSi=%d:%s",index + 1,tagFeatureTail));


        return allFeatures;
    }


    private List<String> localBaseFeatures(Token[] sent, int index, int nToken, String p) {
        List<String> baseFeatures = new ArrayList<String>();

        if (index < nToken) {
            String text;
            String lower;
            String pos;
            String wordShape;

            if (index == -1) {
                text = "<START>";
                pos = "<START>";
                wordShape = "<START>";
                lower = "<START>";
            } else if (index == nToken - 1) {
                text = "<STOP>";
                pos = "<STOP>";
                wordShape = "<STOP>";
                lower = "<STOP>";
            } else {
                Token token = sent[index];
                text = token.text();
                pos = token.pos();
                lower = text.toLowerCase();
                wordShape = getWordShape(text);
            }

            baseFeatures.add(formatWithIndex("W", p, text));
            baseFeatures.add(formatWithIndex("O", p, lower));
            baseFeatures.add(formatWithIndex("P", p, pos));
            baseFeatures.add(formatWithIndex("S", p, wordShape));
        }
        return baseFeatures;
    }


    private String formatWithIndex(String f, String i, String v) {
        return String.format("%s%s=%s", f, i, v);
    }

    /**
     * Get the word
     *
     * @param word
     * @return
     */
    private String getWordShape(String word) {
        StringBuffer shape = new StringBuffer();

        for (int i = 0; i < word.length(); i++) {
            char ch = word.charAt(i);
            if (Character.isLetter(ch)) {
                if (Character.isLowerCase(ch)) {
                    shape.append('a');
                } else {
                    shape.append('A');
                }
            } else if (Character.isDigit(ch)) {
                shape.append('d');
            } else {
                shape.append(ch);
            }
        }

        return shape.toString();
    }

    /**
     * Match gazetteer
     * @param token
     * @param tag
     * @return
     */
    private String gazeMatch(Token token, String tag, Gazetteer gaze) {
        String[] parts = tag.split("-");

        if (parts.length <= 1)
            return "False";
        else {
            if (gaze.contains(parts[1], token.text())) {
               return  "True";
            }
            else
               return  "False";
        }
    }

}
