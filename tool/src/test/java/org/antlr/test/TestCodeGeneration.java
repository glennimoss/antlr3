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

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.stringtemplate.v4.*;

import java.util.*;

/** Code generation testing for different target languages.
 */
@RunWith(Parameterized.class)
public class TestCodeGeneration extends BaseTest {

    @Parameters(name="lang={0}")
    public static Iterable<Object[]> data () {
        return Arrays.asList(new Object[][] {
            { "Java" },
            { "Python3" },
            { "Python" },
        });
    }

    protected STGroupFile targeter;

    public TestCodeGeneration (String lang) {
        this.setTargetLanguage(lang);
        this.targeter = new STGroupFile("target-lang-testing.stg");
        this.targeter.importTemplates(this.testTemplates);
    }


    @Test public void testLabeledNotSetsInLexer() {
        testTarget(this.targeter.getInstanceOf("testLabeledNotSetsInLexer"),
            "a", "z9", "z");
    }

    @Test public void testLabeledSetsInLexer() {
        testTarget(this.targeter.getInstanceOf("testLabeledSetsInLexer"),
            "a", "x", "x");
    }

    @Test public void testLabeledRangeInLexer() {
        testTarget(this.targeter.getInstanceOf("testLabeledSetsInLexer"),
            "a", "x", "x");
    }

    @Test public void testLabeledWildcardInLexer() {
        testTarget(this.targeter.getInstanceOf("testLabeledWildcardInLexer"),
            "a", "x", "x");
    }

    @Test public void testSynpredWithPlusLoop() {
        testTarget(this.targeter.getInstanceOf("testSynpredWithPlusLoop"),
            "a", "xxxxxx", "6");
    }

    @Test public void testGatedPredInCyclicDFA() {
        testTarget(this.targeter.getInstanceOf("testGatedPredInCyclicDFA"),
            "a", "aaaaxaaxaaaaaaaaxaaax", "B\nA\nB\nA");
    }

    @Test public void testORGatedPred() {
      ST t = this.targeter.getInstanceOf("true_value");
      testTarget(this.targeter.getInstanceOf("testORGatedPred"),
          "start", "!bcaczbbzccbcbb", "alt3\nalt2\nalt3\nalt3\nalt3\nalt1\nalt1\nalt3\nalt3\nalt3\nalt2\nalt1");
    }

    @Test public void testSimpleCyclicDFAWithPredicate() {
      testTarget(this.targeter.getInstanceOf("testSimpleCyclicDFAWithPredicate"),
          "a", "xxxyy", "alt2\nalt1");
    }


    protected void testTarget (ST grammarBody, String startRule, String input, String expected) {
        ST targeted = this.targeter.getInstanceOf("targetedGrammar");
        targeted.add("lang", this.targetLanguage);
        targeted.add("body", grammarBody);

        String found = execParser("T.g", targeted.render(), "TParser", "TLexer",
                                  startRule, input, false);
        assertEquals(expected, found.trim());
    }
}
