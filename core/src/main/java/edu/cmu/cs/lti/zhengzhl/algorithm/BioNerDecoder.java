package edu.cmu.cs.lti.zhengzhl.algorithm;

import edu.cmu.cs.lti.zhengzhl.model.Token;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/14/13
 * Time: 1:52 PM
 */
public class BioNerDecoder extends ViterbiDecoder {
    Map<String,Double> weights;

    public BioNerDecoder(String[] stateSet, String sentenceStart, Map<String,Double> weights) {
        super(stateSet, sentenceStart);
        this.weights = weights;
    }

    @Override
    protected double getScore(Token[] observations, int index, String previousState, String currentState) {
        double sum = 0.0      ;
        for (String firedFeature : getFeatures(observations,index,previousState,currentState)){
                if (weights.containsKey(firedFeature)){
                    sum += weights.get(firedFeature);
                }
        }

        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    private List<String> getFeatures(Token[] observations, int index, String previousState, String currentState){
        Token token = observations[index];
        int nToken = observations.length;
        String tagToUse =index == nToken - 1 ? "<STOP>" : currentState;
        String tagFeatureTail = String.format("Ti=%s",tagToUse);
        String text = token.text();
        List<String> allFeatures = new ArrayList<String>();

        //to be finished

        return allFeatures;
    }


}
