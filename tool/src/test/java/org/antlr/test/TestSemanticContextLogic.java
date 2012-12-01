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

    protected Predicate[] p (String ... vals) {
        Predicate[] preds = new Predicate[vals.length];

        for (int i=0; i < vals.length; i++) {
            preds[i] = p(vals[i]);
        }
        return preds;
    }

    protected AND ANDall (String ... terms) {
        return ANDall(p(terms));
    }

    protected AND ANDall (SemanticContext ... terms) {
        return new AND(new HashSet<SemanticContext>(Arrays.asList(terms)));
    }

    protected OR ORall (String ... terms) {
        return ORall(p(terms));
    }

    protected OR ORall (SemanticContext ... terms) {
        return new OR(new HashSet<SemanticContext>(Arrays.asList(terms)));
    }

    @Test
    public void testFactorAnd () {
        SemanticContext[] rset;

        rset = factorAnd(foo, f);
        assertTrue(rset[0] == EMPTY_SEMANTIC_CONTEXT);
        assertTrue(rset[1] == foo);
        assertTrue(rset[2] == f);

        rset = factorAnd(foo, t);
        assertTrue(rset[0] == EMPTY_SEMANTIC_CONTEXT);
        assertTrue(rset[1] == foo);
        assertTrue(rset[2] == t);

        rset = factorAnd(foo, foo);
        assertTrue(rset[0] == foo);
        assertEquals(t, rset[1]);
        assertEquals(t, rset[2]);

        rset = factorAnd(foo, foo2);
        assertEquals(foo3, rset[0]);
        assertEquals(t, rset[1]);
        assertEquals(t, rset[2]);

        rset = factorAnd(foo, bar);
        assertTrue(rset[0] == EMPTY_SEMANTIC_CONTEXT);
        assertTrue(rset[1] == foo);
        assertTrue(rset[2] == bar);

        rset = factorAnd(ANDall(foo, bar), bar2);
        assertEquals(bar3, rset[0]);
        assertTrue(rset[1] == foo);
        assertEquals(t, rset[2]);

        SemanticContext orterm = ORall(foo, bar);
        rset = factorAnd(orterm, bar2);
        assertTrue(rset[0] == EMPTY_SEMANTIC_CONTEXT );
        assertTrue(rset[1] == orterm);
        assertTrue(rset[2] == bar2);

        rset = factorAnd(ANDall(foo, bar), ANDall(foo2, bar2));
        assertEquals(ANDall(foo3, bar3), rset[0]);
        assertEquals(t, rset[1]);
        assertEquals(t, rset[2]);

        rset = factorAnd(ANDall(foo, bar), ANDall(foo2, bar2, baz));
        assertEquals(ANDall(foo3, bar3), rset[0]);
        assertEquals(t, rset[1]);
        assertTrue(rset[2] == baz);
    }

    @Test
    public void testFactorOr () {
        SemanticContext[] rset;

        rset = factorOr(foo, f);
        assertTrue(rset[0] == EMPTY_SEMANTIC_CONTEXT);
        assertTrue(rset[1] == foo);
        assertTrue(rset[2] == f);

        rset = factorOr(foo, t);
        assertTrue(rset[0] == EMPTY_SEMANTIC_CONTEXT);
        assertTrue(rset[1] == foo);
        assertTrue(rset[2] == t);

        rset = factorOr(foo, foo);
        assertTrue(rset[0] == foo);
        assertEquals(f, rset[1]);
        assertEquals(f, rset[2]);

        rset = factorOr(foo, foo2);
        assertEquals(foo3, rset[0]);
        assertEquals(f, rset[1]);
        assertEquals(f, rset[2]);

        rset = factorOr(foo, bar);
        assertTrue(rset[0] == EMPTY_SEMANTIC_CONTEXT);
        assertTrue(rset[1] == foo);
        assertTrue(rset[2] == bar);

        rset = factorOr(ORall(foo, bar), bar2);
        assertEquals(bar3, rset[0]);
        assertTrue(rset[1] == foo);
        assertEquals(f, rset[2]);

        SemanticContext andterm = ANDall(foo, bar);
        rset = factorOr(andterm, bar2);
        assertTrue(rset[0] == EMPTY_SEMANTIC_CONTEXT );
        assertTrue(rset[1] == andterm);
        assertTrue(rset[2] == bar2);

        rset = factorOr(ORall(foo, bar), ORall(foo2, bar2));
        assertEquals(ORall(foo3, bar3), rset[0]);
        assertEquals(f, rset[1]);
        assertEquals(f, rset[2]);

        rset = factorOr(ORall(foo, bar), ORall(foo2, bar2, baz));
        assertEquals(ORall(foo3, bar3), rset[0]);
        assertEquals(f, rset[1]);
        assertTrue(rset[2] == baz);
    }

    @Test
    public void testAnd () {
        SemanticContext r;

        r = and(foo, f);
        assertFalse(r == f);
        assertEquals(f, r);

        r = and(foo, t);
        assertTrue(r == foo);

        r = and(foo, foo);
        assertTrue(r == foo);

        r = and(foo, foo2);
        assertEquals(foo3, r);

        r = and(foo, bar);
        assertEquals(ANDall(foo3, bar3), r);

        r = and(ANDall(foo, bar), bar2);
        assertEquals(ANDall(foo3, bar3), r);

        r = and(ORall(foo, bar), bar2);
        assertEquals(bar3, r);

        r = and(ANDall(foo, bar), ANDall(foo2, bar2));
        assertEquals(ANDall(foo3, bar3), r);

        r = and(ANDall(foo, bar), ANDall(foo2, bar2, baz));
        assertEquals(r, ANDall(foo3, bar3, baz3));
    }

    @Test
    public void testOr () {
        SemanticContext r;

        r = or(foo, f);
        assertTrue(r == foo);

        r = or(foo, t);
        assertFalse(r == t);
        assertEquals(t, r);

        r = or(foo, foo);
        assertTrue(r == foo);

        r = or(foo, foo2);
        assertEquals(foo3, r);

        r = or(foo, bar);
        assertEquals(ORall(foo3, bar3), r);

        r = or(ORall(foo, bar), bar2);
        assertEquals(ORall(foo3, bar3), r);

        r = or(ANDall(foo, bar), bar2);
        assertEquals(bar3, r);

        r = or(ORall(foo, bar), ORall(foo2, bar2));
        assertEquals(ORall(foo3, bar3), r);

        r = or(ORall(foo, bar), ORall(foo2, bar2, baz));
        assertEquals(r, ORall(foo3, bar3, baz3));
    }

    @Test
    public void testSimplification () {
        SemanticContext r;

        r = or(ANDall(foo, bar), ANDall(foo2, bar2, baz));
        assertEquals(r, ANDall(foo3, bar3));

        r = and(ORall(foo, bar), ORall(foo2, bar2, baz));
        assertEquals(r, ORall(foo3, bar3));
    }
}
