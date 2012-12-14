package org.antlr.test;

import org.antlr.analysis.SemanticContext;
import static org.antlr.analysis.SemanticContext.*;
import org.antlr.tool.GrammarAST;
import org.junit.Test;
import org.antlr.grammar.v3.ANTLRParser;
import java.util.Arrays;
import java.util.HashSet;

public class TestSemanticContextLogic extends BaseTest {

    Predicate t = new TruePredicate();
    Predicate f = new FalsePredicate();
    Predicate foo = p("foo");
    Predicate foo2 = p("foo");
    Predicate foo3 = p("foo");
    Predicate bar = p("bar");
    Predicate bar2 = p("bar");
    Predicate bar3 = p("bar");
    Predicate baz = p("baz");
    Predicate baz3 = p("baz");


    protected Predicate p (String val) {
        return new Predicate(new GrammarAST(ANTLRParser.ACTION, val));
    }

    protected SemanticContext[] p (Object ... vals) {
        SemanticContext[] preds = new SemanticContext[vals.length];

        for (int i=0; i < vals.length; i++) {
            if (vals[i] instanceof String) {
                preds[i] = p((String)vals[i]);
            } else {
                preds[i] = (SemanticContext)vals[i];
            }
        }
        return preds;
    }

    protected SemanticContext ANDall (Object ... terms) {
        //begin("ANDall " + Arrays.toString(terms));
        SemanticContext r = and(p(terms));
        //end("-> " + r);
        return r;
    }

    protected SemanticContext ORall (Object ... terms) {
        //begin("ORall " + Arrays.toString(terms));
        SemanticContext r = or(p(terms));
        //end("-> " + r);
        return r;
    }

    @Test public void testNot () {
        SemanticContext exp;

        exp = not(p("a"));
        assertEquals("!(a)", exp.toString());

        exp = not(not(p("a")));
        assertEquals("a", exp.toString());

        exp = not(not(not(p("a"))));
        assertEquals("!(a)", exp.toString());

        exp = not(ANDall(not(ANDall(not(p("a"))))));
        assertEquals("!(a)", exp.toString());

        exp = not(ANDall(not(p("a")), "b", "c", "d"));
        assertEquals("!((!(a))&&(b)&&(c)&&(d))", exp.toString());

        exp = not(ANDall(not(p("a")), not(p("b")), "c", "d"));
        assertEquals("(a)||(b)||(!(c))||(!(d))", exp.toString());

        exp = not(ANDall(not(p("a")), not(p("b")), not(p("c")), "d"));
        assertEquals("(a)||(b)||(c)||(!(d))", exp.toString());

        exp = ANDall("a", not(p("b")), not(p("c")), not(p("d")));
        assertEquals("!((b)||(c)||(d)||(!(a)))", exp.toString());
        assertTrue(exp instanceof NOT);

        exp = ANDall("a", "b", not(p("c")), not(p("d")));
        assertEquals("(a)&&(b)&&(!(c))&&(!(d))", exp.toString());

        exp = ANDall("a", "b", "c", not(p("d")));
        assertEquals("(a)&&(b)&&(c)&&(!(d))", exp.toString());
    }

    @Test public void testAnd () {
        SemanticContext exp;

        exp = ANDall();
        assertEquals("true", exp.toString());
        assertTrue(exp instanceof TruePredicate);

        exp = ANDall("a");
        assertEquals("a", exp.toString());
        assertTrue(exp instanceof Predicate);

        exp = ANDall("a", t);
        assertEquals("a", exp.toString());
        assertTrue(exp instanceof Predicate);

        exp = ANDall("a", f);
        assertEquals("false", exp.toString());
        assertTrue(exp instanceof FalsePredicate);

        exp = ANDall("a", "a");
        assertEquals("a", exp.toString());
        assertTrue(exp instanceof Predicate);

        exp = ANDall("a", not(p("a")));
        assertEquals("false", exp.toString());

        exp = ANDall("a", ANDall("b", "c"));
        assertEquals("(a)&&(b)&&(c)", exp.toString());

        exp = ANDall("a", ANDall("a", "b"), ANDall("c", "a"));
        assertEquals("(a)&&(b)&&(c)", exp.toString());

        exp = ANDall("a", ORall("b", "c"));
        assertEquals("(a)&&((b)||(c))", exp.toString());

        exp = ANDall("a", ORall("a"), ANDall("a"), ORall(ANDall("a")));
        assertEquals("a", exp.toString());
        assertTrue(exp instanceof Predicate);

        exp = ANDall("a", not(ANDall("b", "c")));
        assertEquals("(a)&&(!((b)&&(c)))", exp.toString());

        exp = ANDall("a", not(ANDall(not(ANDall("b", "c", "d")), "e")));
        assertEquals("(a)&&(((b)&&(c)&&(d))||(!(e)))", exp.toString());

        exp = ANDall("a", not(ANDall(not(ORall("b", "c", "d")), "e")));
        assertEquals("(a)&&((b)||(c)||(d)||(!(e)))", exp.toString());

        exp = ANDall("a", not(ANDall(ANDall(not(p("b")), not(p("c")), not(p("d"))), "e")));
        assertEquals("(a)&&((b)||(c)||(d)||(!(e)))", exp.toString());

        exp = ANDall("a", not(ANDall(ORall(not(p("b")), not(p("c")), not(p("d"))), "e")));
        assertEquals("(a)&&(((b)&&(c)&&(d))||(!(e)))", exp.toString());

        exp = ANDall("a", not(ORall("b", "c")));
        assertEquals("(a)&&(!((b)||(c)))", exp.toString());

        exp = ANDall("a", not(ORall(not(ANDall("b", "c", "d")), "e")));
        assertEquals("(a)&&(b)&&(c)&&(d)&&(!(e))", exp.toString());

        exp = ANDall("a", not(ORall(not(ORall("b", "c", "d")), "e")));
        assertEquals("(a)&&((b)||(c)||(d))&&(!(e))", exp.toString());

        exp = ANDall("a", not(ORall(ANDall(not(p("b")), not(p("c")), not(p("d"))), "e")));
        assertEquals("(a)&&((b)||(c)||(d))&&(!(e))", exp.toString());

        exp = ANDall("a", not(ORall(ORall(not(p("b")), not(p("c")), not(p("d"))), "e")));
        assertEquals("(a)&&(b)&&(c)&&(d)&&(!(e))", exp.toString());
    }

    @Test public void testOr () {
        SemanticContext exp;

        exp = ORall();
        assertEquals("false", exp.toString());
        assertTrue(exp instanceof FalsePredicate);

        exp = ORall("a");
        assertEquals("a", exp.toString());
        assertTrue(exp instanceof Predicate);

        exp = ORall("a", t);
        assertEquals("true", exp.toString());
        assertTrue(exp instanceof TruePredicate);

        exp = ORall("a", f);
        assertEquals("a", exp.toString());
        assertTrue(exp instanceof Predicate);

        exp = ORall("a", "a");
        assertEquals("a", exp.toString());
        assertTrue(exp instanceof Predicate);

        exp = ORall("a", not(p("a")));
        assertEquals("true", exp.toString());
        assertTrue(exp instanceof TruePredicate);

        exp = ORall("a", ANDall("b", "c"));
        assertEquals("(a)||((b)&&(c))", exp.toString());

        exp = ORall("a", ORall("b", "c"));
        assertEquals("(a)||(b)||(c)", exp.toString());

        exp = ORall("a", ORall("a", "b"), ORall("c", "a"));
        assertEquals("(a)||(b)||(c)", exp.toString());

        exp = ORall("a", ORall("a"), ANDall("a"), ORall(ANDall("a")));
        assertEquals("a", exp.toString());
        assertTrue(exp instanceof Predicate);

        exp = ORall("a", not(ANDall("b", "c")));
        assertEquals("(a)||(!((b)&&(c)))", exp.toString());

        exp = ORall("a", not(ANDall(not(ANDall("b", "c", "d")), "e")));
        assertEquals("(a)||((b)&&(c)&&(d))||(!(e))", exp.toString());

        exp = ORall("a", not(ANDall(not(ORall("b", "c", "d")), "e")));
        assertEquals("(a)||(b)||(c)||(d)||(!(e))", exp.toString());

        exp = ORall("a", not(ANDall(ANDall(not(p("b")), not(p("c")), not(p("d"))), "e")));
        assertEquals("(a)||(b)||(c)||(d)||(!(e))", exp.toString());

        exp = ORall("a", not(ANDall(ORall(not(p("b")), not(p("c")), not(p("d"))), "e")));
        assertEquals("(a)||((b)&&(c)&&(d))||(!(e))", exp.toString());

        exp = ORall("a", not(ORall("b", "c")));
        assertEquals("(a)||(!((b)||(c)))", exp.toString());

        exp = ORall("a", not(ORall(not(ANDall("b", "c", "d")), "e")));
        assertEquals("(a)||((b)&&(c)&&(d)&&(!(e)))", exp.toString());

        exp = ORall("a", not(ORall(not(ORall("b", "c", "d")), "e")));
        assertEquals("(a)||(((b)||(c)||(d))&&(!(e)))", exp.toString());

        exp = ORall("a", not(ORall(ANDall(not(p("b")), not(p("c")), not(p("d"))), "e")));
        assertEquals("(a)||(((b)||(c)||(d))&&(!(e)))", exp.toString());

        exp = ORall("a", not(ORall(ORall(not(p("b")), not(p("c")), not(p("d"))), "e")));
        assertEquals("(a)||((b)&&(c)&&(d)&&(!(e)))", exp.toString());
    }


    @Test
    public void testAndFactored () {
        SemanticContext exp;

        exp = ANDall("a", ORall("a", "b"), ORall("c", "a"));
        assertEquals("a", exp.toString());
        assertTrue(exp instanceof Predicate);

        exp = ANDall("a", ORall("b", ANDall("c", "a")));
        assertEquals("(a)&&((b)||(c))", exp.toString());

        exp = ANDall("a", ORall("b", ANDall("c", not(p("a")))));
        assertEquals("(a)&&(b)", exp.toString());

        exp = ANDall("a", ORall("b", ANDall("c", not(p("a")))), ORall(not(p("b")), "d"));
        assertEquals("(a)&&(b)&&(d)", exp.toString());

        exp = ANDall("a", "b", ORall("b", ANDall("c", not(p("a")))), ORall(not(p("b")), "d"));
        assertEquals("(a)&&(b)&&(d)", exp.toString());

        exp = ANDall("a", ORall("b", "c"), ORall("d", ORall("b", "c")), ORall("e",  ORall("d", ORall("b", "c"))));
        assertEquals("(a)&&((b)||(c))", exp.toString());

        exp = ANDall(not(p("a")), ORall("b", "a"));
        assertEquals("(!(a))&&(b)", exp.toString());

        exp = ANDall("a", ORall("b", "c"),
                          ORall("d", ANDall("e", ORall("b", "c"))),
                          ORall("f", ANDall("g", ORall("h", ORall("i", "b", "j", "c")))));
        assertEquals("(a)&&((b)||(c))&&((d)||(e))&&((f)||(g))", exp.toString());

        exp = ANDall(not(p("a")), ORall("a", "b", "c"),
                                  ORall("d", ANDall("e", ORall("b", "c"))),
                                  ORall("f", ANDall("g", ORall("h", ORall("i", "b", "j", "c")))));
        assertEquals("(!(a))&&((b)||(c))&&((d)||(e))&&((f)||(g))", exp.toString());
    }

    @Test
    public void testOrFactored () {
        SemanticContext exp;

        exp = ORall("a", ANDall("a", "b"), ANDall("c", "a"));
        assertEquals("a", exp.toString());
        assertTrue(exp instanceof Predicate);

        exp = ORall("a", ANDall("b", ORall("c", "a")));
        assertEquals("(a)||((b)&&(c))", exp.toString());

        exp = ORall("a", ANDall("b", ORall("c", not(p("a")))));
        assertEquals("(a)||(b)", exp.toString());

        exp = ORall("a", ANDall("b", ORall("c", not(p("a")))), ANDall(not(p("b")), "d"));
        assertEquals("(a)||(b)||(d)", exp.toString());

        exp = ORall("a", "b", ANDall("b", ORall("c", not(p("a")))), ANDall(not(p("b")), "d"));
        assertEquals("(a)||(b)||(d)", exp.toString());

        exp = ORall("a", ANDall("b", "c"), ANDall("d", ANDall("b", "c")), ANDall("e",  ANDall("d", ANDall("b", "c"))));
        assertEquals("(a)||((b)&&(c))", exp.toString());

        exp = ORall(not(p("a")), ANDall("b", "a"));
        assertEquals("(!(a))||(b)", exp.toString());

        exp = ORall("a", ANDall("b", "c"),
                         ANDall("d", ORall("e", ANDall("b", "c"))),
                         ANDall("f", ORall("g", ANDall("h", ANDall("i", "b", "j", "c")))));
        assertEquals("(a)||((b)&&(c))||((d)&&(e))||((f)&&(g))", exp.toString());

        exp = ORall(not(p("a")), ANDall("a", "b", "c"),
                                 ANDall("d", ORall("e", ANDall("b", "c"))),
                                 ANDall("f", ORall("g", ANDall("h", ANDall("i", "b", "j", "c")))));
        assertEquals("(!(a))||((b)&&(c))||((d)&&(e))||((f)&&(g))", exp.toString());
    }
}
