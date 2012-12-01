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

public class TestHoistedPredicates extends BaseTest {

    /** Public default constructor used by TestRig */
    public TestHoistedPredicates() {
    }

    @Test public void testHoistedPreds () throws Exception {
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
            ""};

        StringBuilder buf = new StringBuilder();
        for (String s : txt) {
            buf.append(s);
            buf.append('\n');
        }
        Grammar g = new Grammar(buf.toString());
            /*
            "parser grammar P;\n"+
            //"a[a_arg] : sempred[$a_arg + 1] ( sempred[innerarg] A | sempred[innerarg2] B) | sempred[$a_arg + 2] A | {$a_arg}? ID A;\n"+
            "a[a_arg] : sempred[$a_arg + 1] A | sempred[$a_arg + 2] A | {$a_arg}? ID A;\n"+
            "sempred[sem_arg] : {sem_val == $sem_arg}? ID;\n"+
            "b : d D | G | d E;\n"+
            "d : E | F | ;\n"+
            */


        System.out.println(buf.toString());
        runTool(g);
    }

    /*@Test*/ public void testMyGrammar () throws Exception {
        Tool antlr = new Tool();
        antlr.addGrammarFile("/home/gim/git/precog/precog/parser/sql.g");

        Grammar g = antlr.getRootGrammar("/home/gim/git/precog/precog/parser/sql.g");
        runTool(g);
    }

    protected void runTool (Grammar g) throws Exception {
        /*
        g.composite.assignTokenTypes();
        g.addRulesForSyntacticPredicates();
        g.composite.defineGrammarSymbols();
        g.composite.createNFAs();
        g.createLookaheadDFAs();
        */

        DecisionProbe.verbose=true; // make sure we get all error info
        ErrorQueue equeue = new ErrorQueue();
        //ErrorManager.setErrorListener(equeue);
        CodeGenerator generator = new CodeGenerator(newTool(), g, "Python3");
        g.setCodeGenerator(generator);

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

        DOTGenerator dg = new DOTGenerator(g);
        dg.writeDOTFilesForAllRuleNFAs();
        dg.writeDOTFilesForAllDecisionDFAs();

    }
}
