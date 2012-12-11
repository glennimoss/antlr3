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
package org.antlr.test;

import org.antlr.analysis.DFA;
import org.antlr.analysis.DecisionProbe;
import org.antlr.analysis.Label;
import org.antlr.codegen.CodeGenerator;
import org.antlr.misc.BitSet;
import org.antlr.runtime.Token;
import org.antlr.tool.*;
import org.junit.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class SemanticPredicateBaseTest extends BaseTest {

	// S U P P O R T

	public void _template() throws Exception {
		Grammar g = new Grammar(
			"parser grammar t;\n"+
			"a : A | B;");
		String expecting =
			"\n";
		int[] unreachableAlts = null;
		int[] nonDetAlts = new int[] {1,2};
		String ambigInput = "L ID R";
		int[] insufficientPredAlts = new int[] {1};
		int[] danglingAlts = null;
		int numWarnings = 1;
		checkDecision(g, 1, expecting, unreachableAlts,
					  nonDetAlts, ambigInput, insufficientPredAlts,
					  danglingAlts, numWarnings, false);
	}

	protected void checkDecision(Grammar g,
								 int decision,
								 String expecting,
								 int[] expectingUnreachableAlts,
								 int[] expectingNonDetAlts,
								 String expectingAmbigInput,
								 int[] expectingInsufficientPredAlts,
								 int[] expectingDanglingAlts,
								 int expectingNumWarnings,
								 boolean hasPredHiddenByAction)
		throws Exception
	{
		DecisionProbe.verbose=true; // make sure we get all error info
		ErrorQueue equeue = new ErrorQueue();
		ErrorManager.setErrorListener(equeue);
		CodeGenerator generator = new CodeGenerator(newTool(), g, "Java");
		g.setCodeGenerator(generator);
		// mimic actions of org.antlr.Tool first time for grammar g
		if ( g.getNumberOfDecisions()==0 ) {
			g.buildNFA();
			g.createLookaheadDFAs(false);
		}

		if ( equeue.size()!=expectingNumWarnings ) {
			System.err.println("Warnings issued: "+equeue);
		}

		assertEquals("unexpected number of expected problems",
				   expectingNumWarnings, equeue.size());

		DFA dfa = g.getLookaheadDFA(decision);
		FASerializer serializer = new FASerializer(g);
		String result = serializer.serialize(dfa.startState);
		//System.out.print(result);
		List<Integer> unreachableAlts = dfa.getUnreachableAlts();

		// make sure unreachable alts are as expected
		if ( expectingUnreachableAlts!=null ) {
			BitSet s = new BitSet();
			s.addAll(expectingUnreachableAlts);
			BitSet s2 = new BitSet();
			s2.addAll(unreachableAlts);
			assertEquals("unreachable alts mismatch", s, s2);
		}
		else {
			assertEquals("unreachable alts mismatch", 0,
						 unreachableAlts!=null?unreachableAlts.size():0);
		}

		// check conflicting input
		if ( expectingAmbigInput!=null ) {
			// first, find nondet message
			Message msg = getNonDeterminismMessage(equeue.warnings);
			assertNotNull("no nondeterminism warning?", msg);
			assertTrue("expecting nondeterminism; found "+msg.getClass().getName(),
			msg instanceof GrammarNonDeterminismMessage);
			GrammarNonDeterminismMessage nondetMsg =
				getNonDeterminismMessage(equeue.warnings);
			List<Label> labels =
				nondetMsg.probe.getSampleNonDeterministicInputSequence(nondetMsg.problemState);
			String input = nondetMsg.probe.getInputSequenceDisplay(labels);
			assertEquals(expectingAmbigInput, input);
		}

		// check nondet alts
		if ( expectingNonDetAlts!=null ) {
			GrammarNonDeterminismMessage nondetMsg =
				getNonDeterminismMessage(equeue.warnings);
			assertNotNull("found no nondet alts; expecting: "+
										str(expectingNonDetAlts), nondetMsg);
			List<Integer> nonDetAlts =
				nondetMsg.probe.getNonDeterministicAltsForState(nondetMsg.problemState);
			// compare nonDetAlts with expectingNonDetAlts
			BitSet s = new BitSet();
			s.addAll(expectingNonDetAlts);
			BitSet s2 = new BitSet();
			s2.addAll(nonDetAlts);
			assertEquals("nondet alts mismatch", s, s2);
			assertEquals("mismatch between expected hasPredHiddenByAction", hasPredHiddenByAction,
						 nondetMsg.problemState.dfa.hasPredicateBlockedByAction);
		}
		else {
			// not expecting any nondet alts, make sure there are none
			GrammarNonDeterminismMessage nondetMsg =
				getNonDeterminismMessage(equeue.warnings);
			assertNull("found nondet alts, but expecting none", nondetMsg);
		}

		if ( expectingInsufficientPredAlts!=null ) {
			GrammarInsufficientPredicatesMessage insuffPredMsg =
				getGrammarInsufficientPredicatesMessage(equeue.warnings);
			assertNotNull("found no GrammarInsufficientPredicatesMessage alts; expecting: "+
										str(expectingNonDetAlts), insuffPredMsg);
			Map<Integer, Set<Token>> locations = insuffPredMsg.altToLocations;
			Set<Integer> actualAlts = locations.keySet();
			BitSet s = new BitSet();
			s.addAll(expectingInsufficientPredAlts);
			BitSet s2 = new BitSet();
			s2.addAll(actualAlts);
			assertEquals("mismatch between insufficiently covered alts", s, s2);
			assertEquals("mismatch between expected hasPredHiddenByAction", hasPredHiddenByAction,
						 insuffPredMsg.problemState.dfa.hasPredicateBlockedByAction);
		}
		else {
			// not expecting any nondet alts, make sure there are none
			GrammarInsufficientPredicatesMessage nondetMsg =
				getGrammarInsufficientPredicatesMessage(equeue.warnings);
			if ( nondetMsg!=null ) {
				System.out.println(equeue.warnings);
			}
			assertNull("found insufficiently covered alts, but expecting none", nondetMsg);
		}

        assertEquals(expecting, result);
	}

	protected GrammarNonDeterminismMessage getNonDeterminismMessage(List<? extends Message> warnings) {
		for (int i = 0; i < warnings.size(); i++) {
			Message m = warnings.get(i);
			if ( m instanceof GrammarNonDeterminismMessage ) {
				return (GrammarNonDeterminismMessage)m;
			}
		}
		return null;
	}

	protected GrammarInsufficientPredicatesMessage getGrammarInsufficientPredicatesMessage(List<? extends Message> warnings) {
		for (int i = 0; i < warnings.size(); i++) {
			Message m = warnings.get(i);
			if ( m instanceof GrammarInsufficientPredicatesMessage ) {
				return (GrammarInsufficientPredicatesMessage)m;
			}
		}
		return null;
	}

	protected String str(int[] elements) {
		StringBuilder buf = new StringBuilder();
		for (int i = 0; i < elements.length; i++) {
			if ( i>0 ) {
				buf.append(", ");
			}
			int element = elements[i];
			buf.append(element);
		}
		return buf.toString();
	}
}
