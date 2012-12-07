/*
 * [The "BSD license"]
 *  Copyright (c) 2010 Terence Parr
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *  1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *  3. The name of the author may not be used to endorse or promote products
 *      derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 *  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.antlr.tool;

import org.antlr.Tool;
import org.antlr.analysis.*;
import org.antlr.grammar.v3.ANTLRParser;
import org.antlr.misc.Utils;
import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupFile;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

/** The DOT (part of graphviz) generation aspect. */
public class DOTGenerator {
    public static final boolean STRIP_NONREDUCED_STATES = false;

    protected String arrowhead="normal";
    protected String rankdir="LR";

    /** Library of output templates; use <attrname> format */
    public static STGroup stlib = new STGroupFile("org/antlr/tool/templates/dot/dot.stg");

    /** To prevent infinite recursion when walking state machines, record
     *  which states we've visited.  Make a new set every time you start
     *  walking in case you reuse this object.
     */
    protected Set<Object> markedStates = null;

    protected Grammar grammar;

    /** This aspect is associated with a grammar */
    public DOTGenerator(Grammar grammar) {
        this.grammar = grammar;
    }

    public String getAllNFADOT () {
        ST graph = stlib.getInstanceOf("containerGraph");
        graph.add("name", "AllNFAs");
        graph.add("rankdir", this.rankdir);
        for (Rule r : this.grammar.getRules() ) {
            ST sg = this.getDOT(r.startState, "sub", "r_" + r.name);
            sg.add("graphName", "cluster_rule_" + r.name);
            sg.add("graphLabel", r.name);
            graph.add("subgraphs", sg);
        }
        return graph.render();
    }

    public String getAllDFADOT () {
        ST graph = stlib.getInstanceOf("containerGraph");
        graph.add("name", "AllDFAs");
        graph.add("rankdir", this.rankdir);
        for (int i=1; i<= this.grammar.getNumberOfDecisions(); i++) {
            DFA dfa = this.grammar.getLookaheadDFA(i);
            ST sg = this.getDOT(dfa.startState, "sub", "d" + dfa.decisionNumber);
            sg.add("graphName", "cluster_d" + dfa.decisionNumber);
            NFAState s = dfa.getNFADecisionStartState();
            sg.add("graphLabel", s + " -> " + s.getDescription());
            graph.add("subgraphs", sg);
        }
        return graph.render();
    }

    /** Return a String containing a DOT description that, when displayed,
     *  will show the incoming state machine visually.  All nodes reachable
     *  from startState will be included.
     */
    public String getDOT(State startState) {
        return this.getDOT(startState, "di", "").render();
    }

    public ST getDOT(State startState, String graphType, String prefix) {
        if ( startState==null ) {
            return null;
        }
        // The output DOT graph for visualization
        ST dot;
        markedStates = new HashSet<Object>();
        if ( startState instanceof DFAState ) {
            dot = stlib.getInstanceOf("dfa");
            dot.add("startState",
                    Utils.integer(startState.stateNumber));
            dot.add("useBox",
                    Tool.internalOption_ShowNFAConfigsInDFA);
            walkCreatingDFADOT(dot, (DFAState)startState, prefix);
        }
        else {
            dot = stlib.getInstanceOf("nfa");
            dot.add("startState",
                    Utils.integer(startState.stateNumber));
            walkRuleNFACreatingDOT(dot, startState, prefix);
        }
        dot.add("rankdir", rankdir);
        dot.add("graphType", graphType);
        return dot;
    }

    /** Return a String containing a DOT description that, when displayed,
     *  will show the incoming state machine visually.  All nodes reachable
     *  from startState will be included.
    public String getRuleNFADOT(State startState) {
        // The output DOT graph for visualization
        ST dot = stlib.getInstanceOf("nfa");

        markedStates = new HashSet();
        dot.add("startState",
                Utils.integer(startState.stateNumber));
        walkRuleNFACreatingDOT(dot, startState);
        return dot.toString();
    }
     */

    /** Do a depth-first walk of the state machine graph and
     *  fill a DOT description template.  Keep filling the
     *  states and edges attributes.
     */
    protected void walkCreatingDFADOT(ST dot,
                                      DFAState s,
                                      String prefix)
    {
        if ( markedStates.contains(Utils.integer(s.stateNumber)) ) {
            return; // already visited this node
        }

        markedStates.add(Utils.integer(s.stateNumber)); // mark this node as completed.

        // first add this node
        ST st;
        if ( s.isAcceptState() ) {
            st = stlib.getInstanceOf("stopstate");
        }
        else {
            st = stlib.getInstanceOf("state");
        }
        st.add("name", getStateLabel(s));
        st.add("prefix", prefix);
        dot.add("states", st);

        // make a DOT edge for each transition
        for (int i = 0; i < s.getNumberOfTransitions(); i++) {
            Transition edge = s.transition(i);
            /*
            System.out.println("dfa "+s.dfa.decisionNumber+
                " edge from s"+s.stateNumber+" ["+i+"] of "+s.getNumberOfTransitions());
            */
            if ( STRIP_NONREDUCED_STATES ) {
                if ( edge.target instanceof DFAState &&
                    ((DFAState)edge.target).getAcceptStateReachable()!=DFA.REACHABLE_YES )
                {
                    continue; // don't generate nodes for terminal states
                }
            }
            st = stlib.getInstanceOf("edge");
            st.add("label", getEdgeLabel(edge));
            st.add("src", getStateLabel(s));
            st.add("target", getStateLabel(edge.target));
            st.add("arrowhead", arrowhead);
            st.add("prefix", prefix);
            dot.add("edges", st);
            walkCreatingDFADOT(dot, (DFAState)edge.target, prefix); // keep walkin'
        }
    }

    /** Do a depth-first walk of the state machine graph and
     *  fill a DOT description template.  Keep filling the
     *  states and edges attributes.  We know this is an NFA
     *  for a rule so don't traverse edges to other rules and
     *  don't go past rule end state.
     */
    protected void walkRuleNFACreatingDOT(ST dot,
                                          State s,
                                          String prefix)
    {
        if ( markedStates.contains(s) ) {
            return; // already visited this node
        }

        markedStates.add(s); // mark this node as completed.

        // first add this node
        ST stateST;
        if ( s.isAcceptState() ) {
            stateST = stlib.getInstanceOf("stopstate");
        }
        else {
            stateST = stlib.getInstanceOf("state");
        }
        stateST.add("name", getStateLabel(s));
        stateST.add("prefix", prefix);
        dot.add("states", stateST);

        if ( s.isAcceptState() )  {
            return; // don't go past end of rule node to the follow states
        }

        // special case: if decision point, then line up the alt start states
        // unless it's an end of block
        if ( ((NFAState)s).isDecisionState() ) {
            stateST.add("useBox", true);
            GrammarAST n = ((NFAState)s).associatedASTNode;
            if ( n!=null && n.getType()!=ANTLRParser.EOB ) {
                ST rankST = stlib.getInstanceOf("decision_rank");
                rankST.add("prefix", prefix);
                NFAState alt = (NFAState)s;
                while ( alt!=null ) {
                    rankST.add("states", getStateLabel(alt));
                    if ( alt.transition[1] !=null ) {
                        alt = (NFAState)alt.transition[1].target;
                    }
                    else {
                        alt=null;
                    }
                }
                dot.add("decisionRanks", rankST);
            }
        }

        // make a DOT edge for each transition
        ST edgeST;
        for (int i = 0; i < s.getNumberOfTransitions(); i++) {
            Transition edge = s.transition(i);
            if ( edge instanceof RuleClosureTransition ) {
                RuleClosureTransition rr = ((RuleClosureTransition)edge);
                // don't jump to other rules, but display edge to follow node
                edgeST = stlib.getInstanceOf("edge");
                StringBuilder label = new StringBuilder();
                label.append('<');
                if ( rr.rule.grammar != grammar ) {
                    label.append(rr.rule.grammar.name);
                    label.append('.');
                }
                label.append(rr.rule.name);
                if (rr.arguments != null) {
                    label.append('[');
                    label.append(rr.arguments.getText());
                    label.append(']');
                }
                label.append('>');
                edgeST.add("label", label.toString());
                edgeST.add("src", getStateLabel(s));
                edgeST.add("target", getStateLabel(rr.followState));
                edgeST.add("arrowhead", arrowhead);
                edgeST.add("prefix", prefix);
                dot.add("edges", edgeST);
                walkRuleNFACreatingDOT(dot, rr.followState, prefix);
                continue;
            }
            if ( edge.isAction() ) {
                edgeST = stlib.getInstanceOf("action_edge");
            }
            else if ( edge.isEpsilon() ) {
                edgeST = stlib.getInstanceOf("epsilon_edge");
            }
            else {
                edgeST = stlib.getInstanceOf("edge");
            }
            edgeST.add("label", getEdgeLabel(edge));
            edgeST.add("src", getStateLabel(s));
            edgeST.add("target", getStateLabel(edge.target));
            edgeST.add("arrowhead", arrowhead);
            edgeST.add("prefix", prefix);
            dot.add("edges", edgeST);
            walkRuleNFACreatingDOT(dot, edge.target, prefix); // keep walkin'
        }
    }

    public void writeDOTFilesForAllRuleNFAs() throws IOException {
        writeDOTFile(this.grammar.name + ".nfa.dot", getAllNFADOT());
    }

    public void writeDOTFilesForAllDecisionDFAs() throws IOException {
        writeDOTFile(this.grammar.name + ".dfa.dot", getAllDFADOT());
    }

    public void writeDOTFile (String fileName, String dot) throws IOException {
        File outputFile = new File(fileName);
        FileWriter fw = new FileWriter(outputFile);
        fw.write(dot);
        fw.close();
    }

    /** Fix edge strings so they print out in DOT properly;
     *  generate any gated predicates on edge too.
     */
    protected String getEdgeLabel(Transition edge) {
        String label = edge.label.toString(grammar);
        label = Utils.replace(label,"\\", "\\\\");
        label = Utils.replace(label,"\"", "\\\"");
        label = Utils.replace(label,"\n", "\\\\n");
        label = Utils.replace(label,"\r", "");
        if ( label.equals(Label.EPSILON_STR) ) {
            label = "e";
        }
        State target = edge.target;
        if ( !edge.isSemanticPredicate() && target instanceof DFAState ) {
            // look for gated predicates; don't add gated to simple sempred edges
            SemanticContext preds =
                ((DFAState)target).getGatedPredicatesInNFAConfigurations();
            if ( preds!=null ) {
                String predsStr;
                predsStr = "&&{"+
                    preds.genExpr(grammar.generator,
                                  grammar.generator.getTemplates(), null).toString()
                    +"}?";
                label += predsStr;
            }
        }
        return label;
    }

    protected String getStateLabel(State s) {
        if ( s==null ) {
            return "null";
        }
        String stateLabel = String.valueOf(s.stateNumber);
        if ( s instanceof DFAState ) {
            StringBuilder buf = new StringBuilder(250);
            buf.append('s');
            buf.append(s.stateNumber);
            if ( Tool.internalOption_ShowNFAConfigsInDFA ) {
                if ( s instanceof DFAState ) {
                    if ( ((DFAState)s).abortedDueToRecursionOverflow ) {
                        buf.append("\\n");
                        buf.append("abortedDueToRecursionOverflow");
                    }
                }
                Set<Integer> alts = ((DFAState)s).getAltSet();
                if ( alts!=null ) {
                    buf.append("\\n");
                    // separate alts
                    List<Integer> altList = new ArrayList<Integer>();
                    altList.addAll(alts);
                    Collections.sort(altList);
                    Set<NFAConfiguration> configurations = ((DFAState) s).nfaConfigurations;
                    for (int altIndex = 0; altIndex < altList.size(); altIndex++) {
                        Integer altI = altList.get(altIndex);
                        int alt = altI;
                        if ( altIndex>0 ) {
                            buf.append("\\n");
                        }
                        buf.append("alt");
                        buf.append(alt);
                        buf.append(':');
                        // get a list of configs for just this alt
                        // it will help us print better later
                        List<NFAConfiguration> configsInAlt = new ArrayList<NFAConfiguration>();
                        for (NFAConfiguration c : configurations) {
                            if ( c.alt!=alt ) continue;
                            configsInAlt.add(c);
                        }
                        int n = 0;
                        for (int cIndex = 0; cIndex < configsInAlt.size(); cIndex++) {
                            NFAConfiguration c = configsInAlt.get(cIndex);
                            n++;
                            buf.append(c.toString(false));
                            if ( (cIndex+1)<configsInAlt.size() ) {
                                buf.append(", ");
                            }
                            if ( n%5==0 && (configsInAlt.size()-cIndex)>3 ) {
                                buf.append("\\n");
                            }
                        }
                    }
                }
            }
            stateLabel = buf.toString();
        }
        if ( (s instanceof NFAState) && ((NFAState)s).isDecisionState() ) {
            stateLabel = stateLabel+",d="+
                    ((NFAState)s).getDecisionNumber();
            if ( ((NFAState)s).endOfBlockStateNumber!=State.INVALID_STATE_NUMBER ) {
                stateLabel += ",eob="+((NFAState)s).endOfBlockStateNumber;
            }
        }
        else if ( (s instanceof NFAState) &&
            ((NFAState)s).endOfBlockStateNumber!=State.INVALID_STATE_NUMBER)
        {
            NFAState n = ((NFAState)s);
            stateLabel = stateLabel+",eob="+n.endOfBlockStateNumber;
        }
        else if ( s instanceof DFAState && ((DFAState)s).isAcceptState() ) {
            stateLabel = stateLabel+
                    "=>"+((DFAState)s).getUniquelyPredictedAlt();
        }
        return stateLabel;
    }

    public String getArrowheadType() {
        return arrowhead;
    }

    public void setArrowheadType(String arrowhead) {
        this.arrowhead = arrowhead;
    }

    public String getRankdir() {
        return rankdir;
    }

    public void setRankdir(String rankdir) {
        this.rankdir = rankdir;
    }
}
