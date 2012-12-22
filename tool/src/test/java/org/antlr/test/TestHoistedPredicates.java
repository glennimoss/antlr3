package org.antlr.test;

import org.antlr.analysis.DFA;
import org.antlr.analysis.DecisionProbe;
import org.antlr.analysis.Label;
import org.antlr.codegen.CodeGenerator;
import org.antlr.misc.BitSet;
import org.antlr.runtime.Token;
import org.antlr.tool.*;
import org.antlr.Tool;
import org.junit.Test;
import org.stringtemplate.v4.*;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.io.FileInputStream;
import java.io.File;
import java.nio.channels.FileChannel;
import java.nio.MappedByteBuffer;
import java.nio.charset.Charset;

public class TestHoistedPredicates extends SemanticPredicateBaseTest {

    // Tests Modified from TestSemanticPredicateEvaluation

    @Test public void testSimpleCyclicDFAWithPredicate() throws Exception {
        String grammar =
            "grammar foo;\n" +
            "a : b[true];\n" +
            "b[boolean v]\n" +
            "  : {!$v}? 'x'* 'y' {System.out.println(\"alt1\");}\n" +
            "  | {$v}?  'x'* 'y' {System.out.println(\"alt2\");}\n" +
            "  ;\n" ;
        String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
                    "a", "xxxy", false);
        assertEquals("alt2\n", found);
    }


	@Test public void testPredicateValidation() throws Exception {
		String grammar =
			"grammar foo;\n" +
			"@members {\n" +
			"public void reportError(RecognitionException e) {\n" +
			"    System.out.println(\"error: \"+e.toString());\n" +
			"}\n" +
			"}\n" +
			"\n" +
			"a : b[false] 'x';\n" +
			"b[boolean b] : {$b}? 'x';\n" ;
		String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
				    "a", "x", false);
		assertEquals("error: FailedPredicateException(b,{$b}?)\n", found);
	}

	@Test public void testLexerPreds() throws Exception {
		String grammar =
			"grammar foo;" +
            "a : T;\n" +
			"T : (A[false]|B[false])+ ;\n" +
			"fragment A[boolean p] : {$p}? 'a'  {System.out.println(\"token 1\");} ;\n" +
			"fragment B[boolean p] : {!$p}? 'a' {System.out.println(\"token 2\");} ;\n";
		String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
				    "a", "a", false);
		// "a" is ambig; can match both A, B.  Pred says match 2
		assertEquals("token 2\n", found);
	}

	@Test public void testLexerPreds2() throws Exception {
		String grammar =
			"grammar foo;\n" +
			"a : T;\n" +
            "T: (A[true]|B)+ ;\n" +
			"fragment A[boolean p] : {$p}? 'a' {System.out.println(\"token 1\");} ;\n" +
			"fragment B: ('a'|'b')+ {System.out.println(\"token 2\");} ;\n";
        String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
                "a", "a", false);
        // "a" is ambig; can match both A, B.  Pred says match 1
        assertEquals("token 1\n", found);
	}

	@Test public void testLexerPredInExitBranch() throws Exception {
		// $p says it's ok to exit; it has precendence over the !$p loopback branch
		String grammar =
			"grammar foo;" +
			"a : (A|B)+ ;\n" +
            "A : AA[true];\n" +
			"fragment AA[boolean p] : ('a' {System.out.print(\"1\");})*\n" +
			"    {$p}?\n" +
			"    ('a' {System.out.print(\"2\");})* ;\n";
		String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
				    "a", "aaa", false);
		assertEquals("222\n", found);
	}

	@Test public void testLexerPredInExitBranch2() throws Exception {
		String grammar =
			"grammar foo;" +
			"a : (A|B)+ ;\n" +
            "A : AA[true];\n" +
			"fragment AA[boolean p] : ({$p}? 'a' {System.out.print(\"1\");})*\n" +
			"    ('a' {System.out.print(\"2\");})* ;\n";
		String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
				    "a", "aaa", false);
		assertEquals("111\n", found);
	}

	@Test public void testLexerPredInExitBranch3() throws Exception {
		String grammar =
			"grammar foo;" +
			"a : (A|B)+ ;\n" +
            "A : AA[true];\n" +
			"fragment AA[boolean p] : ({$p}? 'a' {System.out.print(\"1\");} | )\n" +
			"    ('a' {System.out.print(\"2\");})* ;\n";
		String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
				    "a", "aaa", false);
		assertEquals("122\n", found);
	}

	@Test public void testLexerPredsInCyclicDFA() throws Exception {
		String grammar =
			"grammar foo;" +
			"a : (A|B)+ ;\n" +
            "A : AA[false];\n" +
			"fragment AA[boolean p]\n" +
			"  : {$p}? ('a')+ 'x' {System.out.println(\"token 1\");} ;\n" +
			"B :       ('a')+ 'x' {System.out.println(\"token 2\");} ;\n";
		String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
				    "a", "aax", false);
		assertEquals("token 2\n", found);
	}

	@Test public void testLexerPredsInCyclicDFA2() throws Exception {
		String grammar =
			"grammar foo;\n" +
			"a : (A|B)+ ;\n" +
            "A : AA[false];\n" +
			"fragment AA[boolean p]\n" +
			"  : {$p}? ('a')+ 'x' ('y')? {System.out.println(\"token 1\");} ;\n" +
			"B :       ('a')+ 'x'        {System.out.println(\"token 2\");} ;\n";
		String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
				    "a", "aax", false);
		assertEquals("token 2\n", found);
	}

	@Test public void testGatedPredExec() throws Exception {
		String grammar =
			"grammar foo;" +
			"a : (A|B)+ ;\n" +
            "A : AA[true];\n" +
			"fragment AA[boolean p] : {$p}?=> 'a' {System.out.println(\"token 1\");} ;\n" +
            "B : BB[false];\n" +
			"fragment BB[boolean p] : {$p}?=>('a'|'b')+ {System.out.println(\"token 2\");} ;\n";
		String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
				    "a", "aa", false);
		// "a" is ambig; can match both A, B.  Pred says match A twice
		assertEquals("token 1\ntoken 1\n", found);
		//assertFalse(true);
	}

	@Test public void testGatedPred2() throws Exception {
		String grammar =
			"grammar foo;\n" +
			"@lexer::members {boolean sig=false;}\n"+
			"a : (A|B)+ ;\n" +
			"A : 'a' {System.out.print(\"A\"); sig=true;} ;\n" +
			"B : 'b' ;\n" +
			"C : CC[sig];\n" +
			"fragment CC[boolean p] : {$p}?=> ('a'|'b') {System.out.print(\"C\");} ;\n";
		String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
				    "a", "aa", false);
		assertEquals("AC\n", found);
	}

	@Test public void testPredWithActionTranslation() throws Exception {
		String grammar =
			"grammar foo;\n" +
			"a : b[2] | c[1] ;\n" +
			"b[int i]\n" +
			"  : {$i==1}?   'a' {System.out.println(\"b alt 1\");}\n" +
			"  | {$b.i==2}? 'a' {System.out.println(\"b alt 2\");}\n" +
			"  ;\n" +
			"c[int j]\n" +
			"  : {$j==1}?   'a' {System.out.println(\"c alt 1\");}\n" +
			"  | {$c.j==2}? 'a' {System.out.println(\"c alt 2\");}\n" +
			"  ;\n";
		String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
				    "a", "aa", false);
		assertEquals("b alt 2\n", found);
	}

	@Test public void testPredicatesOnEOTTarget() throws Exception {
		String grammar =
			"grammar foo; \n" +
			"@lexer::members {boolean p=true, q=false;}" +
			"a : B ;\n" +
			"A: '</'; \n" +
            "B: BB[true];\n" +
			"fragment BB[boolean p]: {$p}? '<!' {System.out.println(\"B\");};\n" +
            "C: CC[false];\n" +
			"fragment CC[boolean q]: {$q}? '<' {System.out.println(\"C\");}; \n" +
			"D: '<';\n" ;
		String found = execParser("foo.g", grammar, "fooParser", "fooLexer",
				    "a", "<!", false);
		assertEquals("B\n", found);
	}




    // Tests Modified from TestSemanticPredicates

	@Test public void testPredsButSyntaxResolves() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | c[a2];\n" +
			"b[p1] : {$p1}? A;\n" +
			"c[p2] : {$p2}? B;");
		String expecting =
			".s0-A->:s1=>1\n" +
			".s0-B->:s2=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testLL_1_Pred() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | b[a2];\n" +
			"b[p] : {$p}? A;");
		String expecting =
			".s0-A->.s1\n" +
			".s1-{a1}?->:s2=>1\n" +
			".s1-{a2}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testLL_1_Pred_forced_k_1() throws Exception {
		// should stop just like before w/o k set.
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a options {k=1;} : b[a1] | b[a2];\n" +
			"b[p] : {$p}? A;");
		String expecting =
			".s0-A->.s1\n" +
			".s1-{a1}?->:s2=>1\n" +
			".s1-{a2}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testLL_2_Pred() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | b[a2];\n" +
			"b[p] : {$p}? A B;");
		String expecting =
			".s0-A->.s1\n" +
			".s1-B->.s2\n" +
			".s2-{a1}?->:s3=>1\n" +
			".s2-{a2}?->:s4=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testPredicatedLoop() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : (b[a1] | b[a2])+;\n" +
			"b[p] : {$p}? A;");
		String expecting =                   // loop back
			".s0-A->.s2\n" +
			".s0-EOF->:s1=>3\n" +
			".s2-{a1}?->:s3=>1\n" +
			".s2-{a2}?->:s4=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testPredicatedToStayInLoop() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : ( b[a] )+ (A)+;\n" +
			"b[p] : {$p}? A;");
		String expecting =
			".s0-A->.s1\n" +
			".s1-{a}?->:s2=>1\n" +
			".s1-{true}?->:s3=>2\n";       // loop back
        checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testAndPredicates() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1, a2] | c[a3];\n" +
			"b[b1, b2] : {$b1}? {$b2}? A;\n" +
			"c[c1] : {$c1}? A;");
		String expecting =
			".s0-A->.s1\n" +
			".s1-{(a1)&&(a2)}?->:s2=>1\n" +
			".s1-{a3}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testOrPredicates() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1, a2] | {a3}? A ;\n" +
			"b[p1, p2] : {$p1}? A | {$p2}? A ;");
		String expecting =
			".s0-A->.s1\n" +
            ".s1-{(a1)||(a2)}?->:s2=>1\n" +
            ".s1-{a3}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testIgnoresHoistingDepthGreaterThanZero() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | b[a2];\n" +
			"b[p] : A {$p}?;");
		String expecting =
			".s0-A->:s1=>1\n";
		checkDecision(g, 1, expecting, new int[] {2},
					  new int[] {1,2}, "A", null, null, 2, false);
	}

	@Test public void testIgnoresPredsHiddenByActions() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | b[a2];\n" +
			"b[p] : {a1} {$p}? A;");
		String expecting =
			".s0-A->:s1=>1\n";
		checkDecision(g, 1, expecting, new int[] {2},
					  new int[] {1,2}, "A", null, null, 2, true);
	}

	@Test public void testIgnoresPredsHiddenByActionsOneAlt() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | c[a2];\n" + // ok since 1 pred visible
			"b[b1] : {$b1}? A;\n" +
            "c[c1] : {a2} {$c1}? A;"); // ok since 1 pred visible
		String expecting =
			".s0-A->.s1\n" +
			".s1-{a1}?->:s2=>1\n" +
			".s1-{true}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null,
					  null, null, null, null, 0, true);
	}

	@Test public void testHoist2() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | c[a2] ;\n" +
			"b[b1] : {$b1}? A ;\n" +
			"c[c1] : {$c1}? A ;\n");
		String expecting =
			".s0-A->.s1\n" +
			".s1-{a1}?->:s2=>1\n" +
			".s1-{a2}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testHoistCorrectContext() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | {a2}? ID ;\n" +
			"b[p] : {$p}? ID | INT ;\n");
		String expecting =  // only tests after ID, not INT :)
			".s0-ID->.s1\n" +
			".s0-INT->:s2=>1\n" +
			".s1-{a1}?->:s2=>1\n" +
			".s1-{a2}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testDefaultPredNakedAltIsLast() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | ID ;\n" +
			"b[p] : {$p}? ID | INT ;\n");
		String expecting =
			".s0-ID->.s1\n" +
			".s0-INT->:s2=>1\n" +
			".s1-{a1}?->:s2=>1\n" +
			".s1-{true}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testDefaultPredNakedAltNotLast() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : ID | b[a1] ;\n" +
			"b[p] : {$p}? ID | INT ;\n");
		String expecting =
			".s0-ID->.s1\n" +
			".s0-INT->:s3=>2\n" +
			".s1-{!(a1)}?->:s2=>1\n" +
			".s1-{a1}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testLeftRecursivePred() throws Exception {
		// No analysis possible. but probably good to fail.  Not sure we really want
		// left-recursion even if guarded with pred.
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"s : a[s1] ;\n" +
			"a[p] : {$p}? a | ID ;\n");
		String expecting =
			".s0-ID->.s1\n" +
			".s1-{s1}?->:s2=>1\n" +
			".s1-{true}?->:s3=>2\n";

		DecisionProbe.verbose=true; // make sure we get all error info
		ErrorQueue equeue = new ErrorQueue();
		ErrorManager.setErrorListener(equeue);
		CodeGenerator generator = new CodeGenerator(newTool(), g, "Java");
		g.setCodeGenerator(generator);
		if ( g.getNumberOfDecisions()==0 ) {
			g.buildNFA();
			g.createLookaheadDFAs(false);
		}

		DFA dfa = g.getLookaheadDFA(1);
		assertEquals(null, dfa); // can't analyze.

		assertEquals("unexpected number of expected problems", 1, equeue.size());
		Message msg = equeue.errors.get(0);
		assertTrue("warning must be a left recursion msg",
				    msg instanceof LeftRecursionCyclesMessage);
	}

	@Test public void testIgnorePredFromLL2AltLastAltIsDefaultTrue() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | A C | c[a2] | d[a3] | A ;\n" +
			"b[b1] : {$b1}? A B;\n" +
            "c[c1] : {$c1}? A;\n" +
            "d[d1] : {$d1}? A;\n");
		// two situations of note:
		// 1. A B syntax is enough to predict that alt, so $b1 is not used
		//    to distinguish it from alts 2..5
		// 2. Alts 3, 4, 5 are nondeterministic with upon A.  $c1, $d1 and the
		//    complement of $c1||$d1 is sufficient to resolve the conflict. Do
		//    not include alt 1's $b1 pred in the "complement of other alts"
		//    because it is not considered nondeterministic with alts 3..5
		String expecting =
			".s0-A->.s1\n" +
			".s1-B->:s2=>1\n" +
			".s1-C->:s3=>2\n" +
			".s1-{a2}?->:s4=>3\n" +
			".s1-{a3}?->:s5=>4\n" +
			".s1-{true}?->:s6=>5\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testIgnorePredFromLL2AltPredUnionNeeded() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | A C | c[a2] | A | d[a3];\n" +
			"b[b1] : {$b1}? A B;\n" +
            "c[c1] : {$c1}? A;\n" +
            "d[d1] : {$d1}? A;\n");
		// two situations of note:
		// 1. A B syntax is enough to predict that alt, so $b1 is not used
		//    to distinguish it from alts 2..5
		// 2. Alts 3, 4, 5 are nondeterministic with upon A.  $c1, $d1 and the
		//    complement of $c1||$d1 is sufficient to resolve the conflict. Do
		//    not include alt 1's $b1 pred in the "complement of other alts"
		//    because it is not considered nondeterministic with alts 3..5
		String expecting =
			".s0-A->.s1\n" +
			".s1-B->:s2=>1\n" +
			".s1-C->:s3=>2\n" +
			".s1-{!((a2)||(a3))}?->:s5=>4\n" +
			".s1-{a2}?->:s4=>3\n" +
			".s1-{a3}?->:s6=>5\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testPredGets2SymbolSyntacticContext() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | A B | C ;\n" +
			"b[p] : {$p}? A B ;\n");
		String expecting =
			".s0-A->.s1\n" +
			".s0-C->:s5=>3\n" +
			".s1-B->.s2\n" +
			".s2-{a1}?->:s3=>1\n" +
			".s2-{true}?->:s4=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testMatchesLongestThenTestPred() throws Exception {
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"a : b[a1] | c[a2] ;\n" +
			"b[b1] : {$b1}? A ;\n" +
			"c[c1] : {$c1}? (A|B)+ ;");
		String expecting =
			".s0-A->.s1\n" +
			".s0-B->:s3=>2\n" +
			".s1-{a1}?->:s2=>1\n" +
			".s1-{a2}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testPredsUsedAfterRecursionOverflow() throws Exception {
		// analysis must bail out due to non-LL(*) nature (ovf)
		// retries with k=1 (but with LL(*) algorithm not optimized version
		// as it has preds)
		Grammar g = new Grammar(
			"parser grammar P;\n"+
			"s : a[s1] '.' | a[s2] ':' ;\n" +
			"a[p] : {$p}? e;\n" +
			"e : '(' e ')' | INT ;\n");
		String expecting =
			".s0-'('->.s1\n" +
			".s0-INT->.s4\n" +
			".s1-{s1}?->:s2=>1\n" +
			".s1-{s2}?->:s3=>2\n" +
			".s4-{s1}?->:s2=>1\n" +
			".s4-{s2}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testPredsUsedAfterK2FailsNoRecursionOverflow() throws Exception {
		// analysis must bail out due to non-LL(*) nature (ovf)
		// retries with k=1 (but with LL(*) algorithm not optimized version
		// as it has preds)
		Grammar g = new Grammar(
			"grammar P;\n" +
			"options {k=2;}\n"+
			"s : a[s1] '.' | a[s2] ':' ;\n" +
			"a[p] : {$p}? e;\n" +
			"e : '(' e ')' | INT ;\n");
		String expecting =
			".s0-'('->.s1\n" +
			".s0-INT->.s6\n" +
			".s1-'('->.s2\n" +
			".s1-INT->.s5\n" +
			".s2-{s1}?->:s3=>1\n" +
			".s2-{s2}?->:s4=>2\n" +
			".s5-{s1}?->:s3=>1\n" +
			".s5-{s2}?->:s4=>2\n" +
			".s6-'.'->:s3=>1\n" +
			".s6-':'->:s4=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testLexerMatchesLongestThenTestPred() throws Exception {
		Grammar g = new Grammar(
			"lexer grammar P;\n"+
			"B : BB[b1];\n" +
			"fragment BB[bb1] : {$bb1}? 'a' ;\n" +
			"C : CC[c1];\n" +
			"fragment CC[cc1] : {$cc1}? ('a'|'b')+ ;");
		String expecting =
			".s0-'a'->.s1\n" +
			".s0-'b'->:s4=>2\n" +
			".s1-'a'..'b'->:s4=>2\n" +
			".s1-<EOT>->.s2\n" +
			".s2-{b1}?->:s3=>1\n" +
			".s2-{c1}?->:s4=>2\n";
		checkDecision(g, 2, expecting, null, null, null, null, null, 0, false);
	}

    @Test public void testGatedPred() throws Exception {
		// gated preds are present on all arcs in predictor
		Grammar g = new Grammar(
			"lexer grammar P;\n"+
            "B : BB[b1];\n" +
			"fragment BB[bb1] : {$bb1}? => 'a' ;\n" +
            "C : CC[c1];\n" +
			"fragment CC[cc1] : {$cc1}? => ('a'|'b')+ ;");
		String expecting =
			".s0-'a'&&{(b1)||(c1)}?->.s1\n" +
            ".s0-'b'&&{c1}?->:s4=>2\n" +
            ".s1-'a'..'b'&&{c1}?->:s4=>2\n" +
            ".s1-<EOT>&&{(b1)||(c1)}?->.s2\n" +
            ".s2-{b1}?->:s3=>1\n" +
            ".s2-{c1}?->:s4=>2\n";
		checkDecision(g, 2, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testGatedPredHoistsAndCanBeInStopState() throws Exception {
		// I found a bug where merging stop states made us throw away
		// a stop state with a gated pred!
		Grammar g = new Grammar(
			"grammar u;\n" +
			"a : b[a1]+ ;\n" +
			"b[p] : 'x' | {$p}?=> 'y' ;");
		String expecting =
			".s0-'x'->:s2=>1\n" +
			".s0-'y'&&{a1}?->:s3=>1\n" +
			".s0-EOF->:s1=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testGatedPredInCyclicDFA() throws Exception {
		Grammar g = new Grammar(
			"lexer grammar P;\n"+
            "A : AA[a1];\n" +
			"fragment AA[aa1] : {$aa1}?=> ('a')+ 'x' ;\n" +
            "B : BB[b1];\n" +
			"fragment BB[bb1] : {$bb1}?=> ('a'|'b')+ 'x' ;");
		String expecting =
			".s0-'a'&&{(a1)||(b1)}?->.s1\n" +
            ".s0-'b'&&{b1}?->:s5=>2\n" +
            ".s1-'a'&&{(a1)||(b1)}?->.s1\n" +
            ".s1-'b'&&{b1}?->:s5=>2\n" +
            ".s1-'x'&&{(a1)||(b1)}?->.s2\n" +
            ".s2-<EOT>&&{(a1)||(b1)}?->.s3\n" +
            ".s3-{a1}?->:s4=>1\n" +
            ".s3-{b1}?->:s5=>2\n";
		checkDecision(g, 3, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testGatedPredNotActuallyUsedOnEdges() throws Exception {
		Grammar g = new Grammar(
			"lexer grammar P;\n"+
			"A : ('a' | AA[a1])\n" +
			"  | 'a' 'b';\n" +
			"fragment AA[aa1] : {$aa1}?=> 'a';\n");
		String expecting1 =
			".s0-'a'->.s1\n" +
			".s1-{!(a1)}?->:s2=>1\n" +  	// Used to disambig subrule
			".s1-{a1}?->:s3=>2\n";
		// rule A decision can't test $aa1 from s0->1 because 'a' is valid
		// for alt1 *and* alt2 w/o $aa1.  Can't test $aa1 from s1 to s3 because
		// we might have passed the first alt of subrule.  The same state
		// is listed in s2 in 2 different configurations: one with and one
		// w/o $aa1.  Can't test therefore.  $aa1||true == true.
		String expecting2 =
			".s0-'a'->.s1\n" +
			".s1-'b'->:s2=>2\n" +
			".s1-<EOT>->:s3=>1\n";
		checkDecision(g, 1, expecting1, null, null, null, null, null, 0, false);
		checkDecision(g, 2, expecting2, null, null, null, null, null, 0, false);
	}

	@Test public void testGatedPredDoesNotForceAllToBeGated() throws Exception {
		Grammar g = new Grammar(
			"grammar w;\n" +
			"a : b[a1] | c[a2] ;\n" +
			"b[b1] : {$b1}? B ;\n" +
			"c[c1] : {$c1}?=> d[$c1] ;\n" +
			"d[d1] : {$d1}? C ;\n");
		String expecting =
			".s0-B->:s1=>1\n" +
			".s0-C&&{a2}?->:s2=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testGatedPredDoesNotForceAllToBeGated2() throws Exception {
		Grammar g = new Grammar(
			"grammar w;\n" +
			"a : b[a1] | c[a2] ;\n" +
			"b[b1] : {$b1}? B ;\n" +
			"c[c1] : {$c1}?=> d[foo($c1)] ;\n" +
			"d[d1] : {$d1}?=> C\n" +
            "      | B;\n");
		String expecting =
			".s0-B->.s1\n" +
			".s0-C&&{(a2)&&(foo((a2)))}?->:s3=>2\n" +
			".s1-{a1}?->:s2=>1\n" +
			".s1-{a2}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testORGatedPred() throws Exception {
		Grammar g = new Grammar(
			"grammar w;\n" +
			"a : b[a1] | c[a2] ;\n" +
			"b[b1] : {$b1}? B ;\n" +
			"c[c1] : {$c1}?=> d[foo($c1), bar($c1)] ;\n" +
			"d[d1,d2] : {$d1}?=> C\n" +
            "         | {$d2}?=> B;\n");
		String expecting =
			".s0-B->.s1\n" +
			".s0-C&&{(a2)&&(foo((a2)))}?->:s3=>2\n" +
			".s1-{(a2)&&(bar((a2)))}?->:s3=>2\n" +
			".s1-{a1}?->:s2=>1\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	/** The following grammar should yield an error that rule 'a' has
	 *  insufficient semantic info pulled from 'b'.
	 */
	@Test public void testIncompleteSemanticHoistedContext() throws Exception {
		ErrorQueue equeue = new ErrorQueue();
		ErrorManager.setErrorListener(equeue);
		Grammar g = new Grammar(
			"parser grammar t;\n"+
			"a : b[a1] | B;\n" +
			"b[p] : {$p}? B | B ;");
		String expecting =
			".s0-B->:s1=>1\n";
		checkDecision(g, 1, expecting, new int[] {2},
					  new int[] {1,2}, "B", new int[] {1}, null, 3, false);
	}

	@Test public void testIncompleteSemanticHoistedContextk2() throws Exception {
		ErrorQueue equeue = new ErrorQueue();
		ErrorManager.setErrorListener(equeue);
		Grammar g = new Grammar(
			"parser grammar t;\n"+
			"a : b[a1] | A B;\n" +
			"b[p] : {$p}? A B | A B ;");
		String expecting =
			".s0-A->.s1\n" +
			".s1-B->:s2=>1\n";
		checkDecision(g, 1, expecting, new int[] {2},
					  new int[] {1,2}, "A B", new int[] {1}, null, 3, false);
	}

	@Test public void testIncompleteSemanticHoistedContextInFOLLOW() throws Exception {
		ErrorQueue equeue = new ErrorQueue();
		ErrorManager.setErrorListener(equeue);
		Grammar g = new Grammar(
			"parser grammar t;\n"+
			"options {k=1;}\n" + // limit to k=1 because it's LL(2); force pred hoist
			"a : A? ;\n" + // need FOLLOW
			"b : X a c[b1] | Y a A ;\n" + // only one A is covered
			"c[c1] : {$c1}? A;"); // only one A is covered
		String expecting =
			".s0-A->:s1=>1\n"; // s0-EOF->s2 branch pruned during optimization
		checkDecision(g, 1, expecting, new int[] {2},
					  new int[] {1,2}, "A", new int[] {2}, null, 3, false);
	}

	@Test public void testIncompleteSemanticHoistedContextInFOLLOWk2() throws Exception {
		ErrorQueue equeue = new ErrorQueue();
		ErrorManager.setErrorListener(equeue);
		Grammar g = new Grammar(
			"parser grammar t;\n"+
			"a : (A B)? ;\n" + // need FOLLOW
			"b : X a c[b1] | Y a A B | Z a ;\n" + // only first alt is covered
            "c[c1] : {$c1}? A B;");
		String expecting =
			".s0-A->.s1\n" +
			".s0-EOF->:s3=>2\n" +
			".s1-B->:s2=>1\n";
		checkDecision(g, 1, expecting, null,
					  new int[] {1,2}, "A B", new int[] {2}, null, 2, false);
	}

	@Test public void testIncompleteSemanticHoistedContextInFOLLOWDueToHiddenPred() throws Exception {
		ErrorQueue equeue = new ErrorQueue();
		ErrorManager.setErrorListener(equeue);
		Grammar g = new Grammar(
			"parser grammar t;\n"+
			"a : (A B)? ;\n" + // need FOLLOW
			"b : X a c[b1] | Y a {a1} c[b2] | Z a ;\n" + // only first alt is covered
            "c[c1] : {$c1}? A B;\n");
		String expecting =
			".s0-A->.s1\n" +
			".s0-EOF->:s3=>2\n" +
			".s1-B->:s2=>1\n";
		checkDecision(g, 1, expecting, null,
					  new int[] {1,2}, "A B", new int[] {2}, null, 2, true);
	}

	/** The following grammar should yield an error that rule 'a' has
	 *  insufficient semantic info pulled from 'b'.  This is the same
	 *  as the previous case except that the D prevents the B path from
	 *  "pinching" together into a single NFA state.
	 *
	 *  This test also demonstrates that just because B D could predict
	 *  alt 1 in rule 'a', it is unnecessary to continue NFA->DFA
	 *  conversion to include an edge for D.  Alt 1 is the only possible
	 *  prediction because we resolve the ambiguity by choosing alt 1.
	 */
	@Test public void testIncompleteSemanticHoistedContext2() throws Exception {
		ErrorQueue equeue = new ErrorQueue();
		ErrorManager.setErrorListener(equeue);
		Grammar g = new Grammar(
			"parser grammar t;\n"+
			"a : b | B;\n" +
			"b : c[b1] | B D ;\n" +
            "c[c1] : {$c1}? B;");
		String expecting =
			".s0-B->:s1=>1\n";
		checkDecision(g, 1, expecting, new int[] {2},
					  new int[] {1,2}, "B", new int[] {1},
					  null, 3, false);
	}

	@Test public void testTooFewSemanticPredicates() throws Exception {
		Grammar g = new Grammar(
			"parser grammar t;\n"+
			"a : b[a1] | c[a2] | A ;\n" +
            "b[b1] : {$b1}? A;\n" +
            "c[c1] : A;");
		String expecting =
			".s0-A->:s1=>1\n";
		checkDecision(g, 1, expecting, new int[] {2,3},
					  new int[] {1,2,3}, "A",
					  null, null, 2, false);
	}

	@Test public void testPredWithK1() throws Exception {
		Grammar g = new Grammar(
			"\tlexer grammar TLexer;\n" +
			"A\n" +
            "options {\n" +
            "  k=1;\n" +
            "}\n" +
			"  : B[a1]\n" +
			"  | B[a2]\n" +
			"  ;\n" +
			"fragment B[b1]\n" +
            "  : {$b1}? ('x')+ '.';\n");
		String expecting =
			".s0-'x'->.s1\n" +
			".s1-{a1}?->:s2=>1\n" +
			".s1-{a2}?->:s3=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

	@Test public void testPredWithArbitraryLookahead() throws Exception {
		Grammar g = new Grammar(
			"\tlexer grammar TLexer;\n" +
			"A : B[a1] | B[a2];\n" +
			"fragment B[b1] : {$b1}? ('x')+ '.';\n");
		String expecting =
			".s0-'x'->.s1\n" +
			".s1-'.'->.s2\n" +
			".s1-'x'->.s1\n" +
			".s2-{a1}?->:s3=>1\n" +
			".s2-{a2}?->:s4=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

    /** For a DFA state with lots of configurations that have the same
	 *  predicate, don't just OR them all together as it's a waste to
	 *  test a||a||b||a||a etc...  ANTLR makes a unique set and THEN
	 *  OR's them together.
	 */
	@Test public void testUniquePredicateOR() throws Exception {
		Grammar g = new Grammar(
			"parser grammar v;\n" +
			"\n" +
			"a[a1, a2]\n" +
            "  : {$a1}? b[$a2]\n" +
			"  | {$a2}? b[$a1]\n" +
			"  ;\n" +
			"\n" +
			"b[b1] : {$b1}? (X)+ ;\n" +
			"\n" +
			"c : a[c1, c1]\n" +
			"  | b[c2]\n" +
			"  ;\n");
		String expecting =
			".s0-X->.s1\n" +
            ".s1-{c1}?->:s2=>1\n" +
            ".s1-{c2}?->:s3=>2\n";
		checkDecision(g, 3, expecting, null, null, null, null, null, 0, false);
	}

    @Test public void testSemanticContextPreventsEarlyTerminationOfClosure() throws Exception {
		Grammar g = new Grammar(
			"parser grammar T;\n" +
			"a : loop[a1, a2, a3] SEMI | ID SEMI\n" +
			"  ;\n" +
			"loop[while, do, for]\n" +
			"    : {$while}? ID\n" +
			"    | {$do}? ID\n" +
			"    | {$for}? ID\n" +
			"    ;");
		String expecting =
			".s0-ID->.s1\n" +
            ".s1-SEMI->.s2\n" +
            ".s2-{(a1)||(a2)||(a3)}?->:s3=>1\n" +
            ".s2-{true}?->:s4=>2\n";
		checkDecision(g, 1, expecting, null, null, null, null, null, 0, false);
	}

    /*@Test*/ public void testHoistedPreds () throws Exception {
        //String[] txt = {
            //"grammar KW;",
            ////"options { output=AST; }",
            //"s : a | b['AWESOME'];",
            //"a : ID;",
            //"b[foo] : k[$foo + '_COOLBEANS'];",
            //"k[kw] /*returns [t]*/ : {self.input.LT(1).text.lower() == $kw.lower()}? ID /*{ t = $ID }*/;",
            //"j: (first=k['FUN'] | first=k['TIMES'] | first=k['BEANS']) (second=k['FOO'] | second=k['BAR'] | second=k['BAZ']) (repeat[$first.text] | repeat[$second.text]);",
            //"repeat[t]: {self.input.LT(1).text == $t.text}? ID;",
            //"ID : ( 'a' .. 'z' ) ( 'a' .. 'z' | '0' .. '9' | '_' | '$' | '#' )*;",
            //""};
        String[] txt = {
        /*
            "grammar KW;",
            "s : a | b['z'] | c[0];",
            "a : b['a'] | b['b'] | b['c'];",
            "b[b_arg] : {$b_arg == 'a'}? c[+1]",
            "         | {$b_arg == 'b'}? c[-1]",
            "         | {$b_arg == 'c'}? c[+2];",
            "c[c_arg] : {$c_arg == self.input.LT(1).text}?=> ID",
            "         | NUM;",
            "ID : ( 'a' .. 'z' ) ( 'a' .. 'z' | '0' .. '9' | '_' | '$' | '#' )*;",
            "NUM : '0' .. '9';",
        */

            "grammar foo;",
            "a : b[true];",
            "b[v]",
            "  : {!$v}? 'x'* 'y' {System.out.println(\"alt1\");}",
            "  | {$v}?  'x'* 'y' {System.out.println(\"alt2\");}",
            "  ;",


            ""};

        StringBuilder buf = new StringBuilder();
        for (String s : txt) {
            buf.append(s);
            buf.append('\n');
        }

            /*
            "parser grammar P;\n"+
            //"a[a_arg] : sempred[$a_arg + 1] ( sempred[innerarg] A | sempred[innerarg2] B) | sempred[$a_arg + 2] A | {$a_arg}? ID A;\n"+
            "a[a_arg] : sempred[$a_arg + 1] A | sempred[$a_arg + 2] A | {$a_arg}? ID A;\n"+
            "sempred[sem_arg] : {sem_val == $sem_arg}? ID;\n"+
            "b : d D | G | d E;\n"+
            "d : E | F | ;\n"+
            */


        runTool(buf.toString());
    }

    @Test public void testMyGrammar () throws Exception {
        //String grammarFile = "/home/gim/git/precog/precog/parser/sql.g";
        String grammarFile = "/tmp/antlr-org.antlr.test.TestCodeGeneration-1356048806490/T.g";
        Tool antlr = new Tool();
        antlr.addGrammarFile(grammarFile);

        Grammar g = antlr.getRootGrammar(grammarFile);
        runTool(g);
    }

    protected void runTool (String txt) throws Exception {
        Grammar g = new Grammar(txt);
        System.out.println(txt);

        runTool(g);
    }

    protected void runToolForLexer (String txt) throws Exception {
        Grammar g = new Grammar(txt);
        String lexerTxt = g.getLexerGrammar();
        runTool(lexerTxt);
    }

    protected void runTool (Grammar g) throws Exception {
        DecisionProbe.verbose=true; // make sure we get all error info
        ErrorQueue equeue = new ErrorQueue();
        //ErrorManager.setErrorListener(equeue);
        //CodeGenerator generator = new CodeGenerator(newTool(), g, "Python3");
        CodeGenerator generator = new CodeGenerator(newTool(), g, "Java");
        g.setCodeGenerator(generator);

        g.composite.assignTokenTypes();
        g.addRulesForSyntacticPredicates();
        g.composite.defineGrammarSymbols();
        g.composite.createNFAs();
        g.createLookaheadDFAs();

        /*
        System.out.println(g.getNumberOfDecisions());
        System.out.println("digraph Grammar {");
        for (int i=1; i<= g.getNumberOfDecisions(); i++) {
          System.out.println(dg.getDOT(g.getLookaheadDFA(i).startState));
        }
        System.out.println("}");
        */


        /*
        // mimic actions of org.antlr.Tool first time for grammar g
        if ( g.getNumberOfDecisions()==0 ) {
            g.buildNFA();
            g.createLookaheadDFAs(false);
        }
        */

        /*
        ST out = generator.genRecognizer();
        if (ErrorManager.getNumErrors() == 0) {
          for (Grammar.Decision d : g.getDecisions()) {
            System.out.println(d.startState);
            System.out.println(d.blockAST.toStringTree());

            int i = 1;
            GrammarAST alt;
            while ((alt = d.blockAST.getBlockALT(i++)) != null) {
              System.out.println(alt.toStringTree());
            }
            System.out.println(d.dfa);
          }
          System.out.println(out.render());
        }
        */

        DOTGenerator dg = new DOTGenerator(g);
        dg.writeDOTFilesForAllRuleNFAs();
        dg.writeDOTFilesForAllDecisionDFAs();

    }
}
