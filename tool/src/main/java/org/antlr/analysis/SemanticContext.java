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
package org.antlr.analysis;

import org.antlr.codegen.CodeGenerator;
import org.antlr.grammar.v3.ANTLRParser;
import org.antlr.grammar.v3.ActionTranslator;
import org.antlr.tool.Grammar;
import org.antlr.tool.GrammarAST;
import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;

import java.util.*;

/** A binary tree structure used to record the semantic context in which
 *  an NFA configuration is valid.  It's either a single predicate or
 *  a tree representing an operation tree such as: p1&&p2 or p1||p2.
 *
 *  For NFA o-p1->o-p2->o, create tree AND(p1,p2).
 *  For NFA (1)-p1->(2)
 *           |       ^
 *           |       |
 *          (3)-p2----
 *  we will have to combine p1 and p2 into DFA state as we will be
 *  adding NFA configurations for state 2 with two predicates p1,p2.
 *  So, set context for combined NFA config for state 2: OR(p1,p2).
 *
 *  I have scoped the AND, NOT, OR, and Predicate subclasses of
 *  SemanticContext within the scope of this outer class.
 *
 *  July 7, 2006: TJP altered OR to be set of operands. the Binary tree
 *  made it really hard to reduce complicated || sequences to their minimum.
 *  Got huge repeated || conditions.
 */
public abstract class SemanticContext {
    /** Create a default value for the semantic context shared among all
     *  NFAConfigurations that do not have an actual semantic context.
     *  This prevents lots of if!=null type checks all over; it represents
     *  just an empty set of predicates.
     */
    public static final SemanticContext EMPTY_SEMANTIC_CONTEXT = new Predicate(Predicate.INVALID_PRED_VALUE);

    /** Given a semantic context expression tree, return a tree with all
     *  nongated predicates set to true and then reduced.  So p&&(q||r) would
     *  return p&&r if q is nongated but p and r are gated.
     */
    public abstract SemanticContext getGatedPredicateContext();

    /** Generate an expression that will evaluate the semantic context,
     *  given a set of output templates.
     */
    public abstract ST genExpr(CodeGenerator generator,
            STGroup templates,
            DFA dfa);

    public abstract boolean hasUserSemanticPredicate(); // user-specified sempred {}? or {}?=>
    public abstract boolean isSyntacticPredicate();

    /** Notify the indicated grammar of any syn preds used within this context */
    public void trackUseOfSyntacticPredicates(Grammar g) {
    }

    protected SemanticContext factored () {
        return this;
    }

    protected SemanticContext simplify () {
        return this;
    }

    protected SemanticContext substitute (Set<SemanticContext> terms, SemanticContext replacement) {
        if (terms.contains(this)) {
            //print("Found a replacement: " + this + " -> " + replacement);
            return replacement;
        } else if (terms.contains(not(this))) {
            replacement = not(replacement);
            //print("Found a replacement: NOT " + this + " -> " + replacement);
            return replacement;
        }

        return this._substitute(terms, replacement);
    }

    protected SemanticContext _substitute (Set<SemanticContext> terms, SemanticContext replacement) {
        return this;
    }

    public static class Predicate extends SemanticContext {
        /** The AST node in tree created from the grammar holding the predicate */
        public GrammarAST predicateAST;

        /** Is this a {...}?=> gating predicate or a normal disambiguating {..}?
         *  If any predicate in expression is gated, then expression is considered
         *  gated.
         *
         *  The simple Predicate object's predicate AST's type is used to set
         *  gated to true if type==GATED_SEMPRED.
         */
        protected boolean gated = false;

        /** syntactic predicates are converted to semantic predicates
         *  but synpreds are generated slightly differently.
         */
        protected boolean synpred = false;

        public static final int INVALID_PRED_VALUE = -2;
        public static final int FALSE_PRED = 0;
        public static final int TRUE_PRED = ~0;

        /** sometimes predicates are known to be true or false; we need
         *  a way to represent this without resorting to a target language
         *  value like true or TRUE.
         */
        protected int constantValue = INVALID_PRED_VALUE;

        public Predicate(int constantValue) {
            predicateAST = new GrammarAST();
            this.constantValue=constantValue;
        }

        public Predicate(GrammarAST predicate) {
            this.predicateAST = predicate;
            this.gated =
                predicate.getType()==ANTLRParser.GATED_SEMPRED ||
                predicate.getType()==ANTLRParser.SYN_SEMPRED ;
            this.synpred =
                predicate.getType()==ANTLRParser.SYN_SEMPRED ||
                predicate.getType()==ANTLRParser.BACKTRACK_SEMPRED;
        }

        public Predicate(Predicate p) {
            this.predicateAST = p.predicateAST;
            this.gated = p.gated;
            this.synpred = p.synpred;
            this.constantValue = p.constantValue;
        }

        /** Two predicates are the same if they are literally the same
         *  text rather than same node in the grammar's AST.
         *  Or, if they have the same constant value, return equal.
         *  As of July 2006 I'm not sure these are needed.
         */
        @Override
        public boolean equals(Object o) {
            if ( !(o instanceof Predicate) ) {
                return false;
            }

            Predicate other = (Predicate)o;
            if (this.constantValue != other.constantValue){
                return false;
            }

            if (this.constantValue != INVALID_PRED_VALUE) {
                return true;
            }

            return this.toString().equals(other.toString());
        }

        @Override
        public int hashCode() {
            if (constantValue != INVALID_PRED_VALUE){
                return constantValue;
            }

            if ( predicateAST == null ) {
                return 0;
            }

            return this.toString().hashCode();
        }

        @Override
        public ST genExpr(CodeGenerator generator,
                STGroup templates,
                DFA dfa)
        {
            return genExpr(generator, templates, dfa, this.predicateAST);
        }

        protected ST genExpr(CodeGenerator generator,
                STGroup templates,
                DFA dfa,
                GrammarAST actionTree)
        {
            ST eST;
            if (templates != null) {
                if ( synpred ) {
                    eST = templates.getInstanceOf("evalSynPredicate");
                }
                else {
                    eST = templates.getInstanceOf("evalPredicate");
                }
            } else {
                eST = new ST("<pred>");
            }

            if (generator != null) {
                if (dfa != null && !synpred) {
                    generator.grammar.decisionsWhoseDFAsUsesSemPreds.add(dfa);
                }
                eST.add("pred", generator.translateAction(actionTree.enclosingRuleName, actionTree));
                eST.add("description", generator.target.getTargetStringLiteralFromString(this.toString()));
            } else {
                eST.add("pred", actionTree.getText());
            }
            return eST;
        }

        protected String hoistExpr (CodeGenerator generator, STGroup templates)
        {
            return this.toString();
        }

        @Override
        public SemanticContext getGatedPredicateContext() {
            if ( gated ) {
                return this;
            }
            return null;
        }

        @Override
        public boolean hasUserSemanticPredicate() { // user-specified sempred
            return predicateAST !=null &&
                ( predicateAST.getType()==ANTLRParser.GATED_SEMPRED ||
                  predicateAST.getType()==ANTLRParser.SEMPRED );
        }

        @Override
        public boolean isSyntacticPredicate() {
            return predicateAST !=null &&
                ( predicateAST.getType()==ANTLRParser.SYN_SEMPRED ||
                  predicateAST.getType()==ANTLRParser.BACKTRACK_SEMPRED );
        }

        @Override
        public void trackUseOfSyntacticPredicates(Grammar g) {
            if ( synpred ) {
                g.synPredNamesUsedInDFA.add(predicateAST.getText());
            }
        }

        @Override
        public String toString() {
            if ( predicateAST ==null ) {
                return "<nopred>";
            }
            return predicateAST.getText();
        }

        public String getEnclosingRuleName () {
            return this.predicateAST.enclosingRuleName;
        }

        public int getAltNum () {
            return this.predicateAST.outerAltNum;
        }
    }


    public static class TruePredicate extends Predicate {
        public TruePredicate() {
            super(TRUE_PRED);
        }

        @Override
        public ST genExpr(CodeGenerator generator,
                STGroup templates,
                DFA dfa)
        {
            if ( templates!=null ) {
                return templates.getInstanceOf("true_value");
            }
            return new ST("true");
        }

        @Override
        public boolean hasUserSemanticPredicate() {
            return false; // not user specified.
        }

        @Override
        public String toString() {
            return "true"; // not used for code gen, just DOT and print outs
        }
    }


    public static class FalsePredicate extends Predicate {
        public FalsePredicate() {
            super(FALSE_PRED);
        }

        @Override
        public ST genExpr(CodeGenerator generator,
                STGroup templates,
                DFA dfa)
        {
            if ( templates!=null ) {
                return templates.getInstanceOf("false");
            }
            return new ST("false");
        }

        @Override
        public boolean hasUserSemanticPredicate() {
            return false; // not user specified.
        }

        @Override
        public String toString() {
            return "false"; // not used for code gen, just DOT and print outs
        }
    }


    public static class NOT extends SemanticContext {
        protected SemanticContext ctx;
        protected NOT (SemanticContext ctx) {
            this.ctx = ctx;
        }

        @Override
        public ST genExpr(CodeGenerator generator,
                STGroup templates,
                DFA dfa)
        {
            ST eST;
            if ( templates!=null ) {
                eST = templates.getInstanceOf("notPredicate");
            }
            else {
                eST = new ST("!(<pred>)");
            }
            eST.add("pred", ctx.genExpr(generator,templates,dfa));
            return eST;
        }

        @Override
        public SemanticContext getGatedPredicateContext() {
            SemanticContext p = ctx.getGatedPredicateContext();
            if ( p==null ) {
                return null;
            }
            return new NOT(p);
        }

        @Override
        public boolean hasUserSemanticPredicate() {
            return ctx.hasUserSemanticPredicate();
        }

        @Override
        public boolean isSyntacticPredicate() {
            return ctx.isSyntacticPredicate();
        }

        @Override
        public void trackUseOfSyntacticPredicates(Grammar g) {
            ctx.trackUseOfSyntacticPredicates(g);
        }

        @Override
        public boolean equals(Object object) {
            if ( !(object instanceof NOT) ) {
                return false;
            }
            return this.ctx.equals(((NOT)object).ctx);
        }

        @Override
        public int hashCode() {
            return ~ctx.hashCode();
        }

        @Override
        public String toString() {
            return "!("+ctx+")";
        }

        @Override
        protected SemanticContext simplify () {
            SemanticContext simple = this;
            if (this.ctx instanceof NOT) {
                simple = ((NOT)this.ctx).ctx;
            } else if (this.ctx instanceof CommutativePredicate) {
                CommutativePredicate comm = (CommutativePredicate)this.ctx;

                Set<SemanticContext> notTerms = new LinkedHashSet<SemanticContext>();
                Set<SemanticContext> notNotTerms = new LinkedHashSet<SemanticContext>();

                int n = comm.operands.size();
                int m = 0;
                for (SemanticContext term : comm.operands) {
                    if (term instanceof NOT) {
                        notTerms.add(((NOT)term).ctx);
                    } else {
                        notNotTerms.add(term);
                    }
                }

                if (notTerms.size() >= notNotTerms.size()) {
                    // NOT the notNotTerms and add to notTerms
                    for (SemanticContext term : notNotTerms) {
                        notTerms.add(not(term));
                    }
                    simple = comm.inverse().make(notTerms);
                }
            } else if (this.ctx instanceof TruePredicate) {
                simple = new FalsePredicate();
            } else if (this.ctx instanceof FalsePredicate) {
                simple = new TruePredicate();
            }
            return simple;
        }

        @Override
        protected SemanticContext _substitute (Set<SemanticContext> terms, SemanticContext replacement) {
            SemanticContext subbed = this.ctx.substitute(terms, replacement);
            if (subbed == this.ctx) {
                return this;
            }
            return not(subbed);
        }
    }


    public static class HoistedPredicate extends Predicate {
        protected Predicate semPred;
        public GrammarAST arguments;
        public Grammar grammar;

        public HoistedPredicate (Predicate semPred, GrammarAST arguments, Grammar grammar) {
            super(semPred);
            this.semPred = semPred;
            this.arguments = arguments;
            this.grammar = grammar;
        }


        @Override
        public ST genExpr(CodeGenerator generator,
                STGroup templates,
                DFA dfa)
        {
            if (generator == null && this.grammar != null) {
                generator = this.grammar.getCodeGenerator();
            }
            if (templates == null && generator != null) {
                templates = generator.getTemplates();
            }
            String hoistedExpr = hoistExpr(generator, templates);
            GrammarAST actionTree = new GrammarAST();
            // Borrow all contextual info from the arguments tree.
            actionTree.initialize(this.arguments);
            // But then replace the token with our actual expression text
            actionTree.initialize(ANTLRParser.ACTION, hoistedExpr);
            return genExpr(generator, templates, dfa, actionTree);
        }

        @Override
        protected String hoistExpr (CodeGenerator generator, STGroup templates)
        {
            String semExpr = this.semPred.hoistExpr(generator, templates);
            String hoistedExpr;

            if (generator != null) {
                hoistedExpr = generator.translateHoistedPredicate(this.semPred.getEnclosingRuleName(),
                        this.semPred.getAltNum(), semExpr, this.arguments);

                // Normalize the expression. If the entire string is wrapped in
                // grouping operators (parenthesis in most targets) remove them
                // so we don't have unnecessary nested groupings.
                if (templates != null) {
                    String[] grouping = templates.getInstanceOf("evalPredicateGroup")
                        .add("pred", "\u001f").render().split("\u001f", 2);
                    if (hoistedExpr.startsWith(grouping[0]) && hoistedExpr.endsWith(grouping[1])) {
                        hoistedExpr = hoistedExpr.substring(grouping[0].length(),
                                hoistedExpr.length() - grouping[1].length());
                    }
                }
            } else {
                hoistedExpr = this.toString();
            }
            return hoistedExpr;
        }

        @Override
        public String toString() {
            if (this.grammar != null) {
                // By generating the final exrpession here, if two
                // HoistedPredicates have different constructions but reduce to
                // the exact same string, they can be considered equal for
                // simplification.
                CodeGenerator gen = this.grammar.getCodeGenerator();
                return this.hoistExpr(gen, gen.getTemplates());
            }
            StringBuilder buf = new StringBuilder();
            buf.append('(');
            buf.append(this.semPred.toString());
            buf.append(")(");
            buf.append(this.arguments.getText());
            buf.append(')');
            return buf.toString();
        }

        @Override
        public String getEnclosingRuleName () {
            return this.arguments.enclosingRuleName;
        }

        @Override
        public int getAltNum () {
            return this.arguments.outerAltNum;
        }
    }


    public static abstract class CommutativePredicate extends SemanticContext {
        protected final Set<SemanticContext> operands = new LinkedHashSet<SemanticContext>();
        protected int hashcode;

        public CommutativePredicate () {}

        public CommutativePredicate (SemanticContext ... ops) {
            this(Arrays.asList(ops));
        }

        public CommutativePredicate (Collection<SemanticContext> contexts){
            for (SemanticContext context : contexts){
                if (context != null && !context.equals(EMPTY_SEMANTIC_CONTEXT)) {
                    if (context.getClass() == this.getClass()) {
                        this.operands.addAll(((CommutativePredicate)context).operands);
                    } else {
                        this.operands.add(context);
                    }
                }
            }

            this.hashcode = this.calculateHashCode();
        }

        @Override
        public SemanticContext getGatedPredicateContext() {
            SemanticContext result = null;
            for (SemanticContext semctx : operands) {
                SemanticContext gatedPred = semctx.getGatedPredicateContext();
                if ( gatedPred!=null ) {
                    result = make(result, gatedPred).simplify();
                }
            }
            return result;
        }

        @Override
        public boolean hasUserSemanticPredicate() {
            for (SemanticContext semctx : operands) {
                if ( semctx.hasUserSemanticPredicate() ) {
                    return true;
                }
            }
            return false;
        }

        @Override
        public boolean isSyntacticPredicate() {
            for (SemanticContext semctx : operands) {
                if ( semctx.isSyntacticPredicate() ) {
                    return true;
                }
            }
            return false;
        }

        @Override
        public void trackUseOfSyntacticPredicates(Grammar g) {
            for (SemanticContext semctx : operands) {
                semctx.trackUseOfSyntacticPredicates(g);
            }
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;

            if (obj.getClass() == this.getClass()) {
                CommutativePredicate commutative = (CommutativePredicate)obj;
                Set<SemanticContext> otherOperands = commutative.operands;
                if (operands.size() != otherOperands.size())
                    return false;

                return operands.containsAll(otherOperands);
            }

            if (obj instanceof NOT) {
                NOT not = (NOT)obj;
                if (not.ctx instanceof CommutativePredicate && not.ctx.getClass() != this.getClass()) {
                    Set<SemanticContext> otherOperands = ((CommutativePredicate)not.ctx).operands;
                    if (operands.size() != otherOperands.size())
                        return false;

                    ArrayList<SemanticContext> temp = new ArrayList<SemanticContext>(operands.size());
                    for (SemanticContext context : otherOperands) {
                        temp.add(not(context));
                    }

                    return operands.containsAll(temp);
                }
            }

            return false;
        }

        @Override
        public int hashCode(){
            return hashcode;
        }

        @Override
        public String toString() {
            StringBuilder buf = new StringBuilder();
            int i = 0;
            for (SemanticContext semctx : operands) {
                if ( i>0 ) {
                    buf.append(getOperatorString());
                }
                buf.append("(");
                buf.append(semctx.toString());
                buf.append(")");
                i++;
            }
            return buf.toString();
        }

        public abstract String getOperatorString();

        public abstract int calculateHashCode();

        /**
         * The absorbing term for this operator.
         *
         * By definition if "a" is the absorbing term and "b" is any other
         * operand, then "a op b == a".
         */
        protected abstract SemanticContext getAbsorbingTerm ();

        /**
         * The identity term for this operator.
         *
         * By definition if "a" is the identity term and "b" is any other
         * operand, then "a op b == b".
         */
        protected abstract SemanticContext getIdentityTerm ();

        /**
         * The inverse operator of this operator.
         */
        protected abstract CommutativePredicate inverse ();

        protected abstract SemanticContext make (SemanticContext a, SemanticContext b);
        protected abstract SemanticContext make (Collection<SemanticContext> ops);

        protected SemanticContext factored () {
            //begin(this.getOperatorString() + ".Factoring: " + this);
            List<Set<SemanticContext>> ops = new ArrayList<Set<SemanticContext>>(this.operands.size());

            Set<SemanticContext> commonTerms = null;
            CommutativePredicate inverse = this.inverse();
            for (SemanticContext op : this.operands) {
                Set<SemanticContext> fops = inverse.getFactoringOperands(op);
                ops.add(fops);
                if (commonTerms == null) {
                    commonTerms = new LinkedHashSet<SemanticContext>(fops);
                } else {
                    //print("Union: " + commonTerms + ", " + fops);

                    commonTerms.retainAll(fops);
                    //print("== " + commonTerms);
                }

                if (commonTerms.isEmpty()) {
                    // Nothing in common, break early
                    break;
                }
            }

            SemanticContext self = this;
            if (commonTerms.isEmpty()) {
                // Already no terms in common, no need to do more.
                //print("No common terms");
            } else {
                Set<SemanticContext> factors = new LinkedHashSet<SemanticContext>(ops.size());
                for (Set<SemanticContext> fops : ops) {
                    fops.removeAll(commonTerms);
                    factors.add(inverse.make(fops));
                }
                //print("Removed common terms: " + factors);

                self = inverse.make(inverse.make(commonTerms), this.make(factors));
            }

            if (self instanceof CommutativePredicate) {
                CommutativePredicate myself = (CommutativePredicate)self;

                Set<SemanticContext> terms = new LinkedHashSet<SemanticContext>(myself.operands.size());
                SemanticContext identity = myself.getIdentityTerm();
                SemanticContext absorb = myself.getAbsorbingTerm();

                Set<SemanticContext> myops = new LinkedHashSet<SemanticContext>(myself.operands);
                for (SemanticContext term : myself.operands) {
                    myops.remove(term);

                    terms.add(term.substitute(myops, identity));

                    myops.add(term);
                }

                if (!myself.operands.equals(terms)) {
                    //print("Terms changed...");
                    self = myself.make(terms).factored();
                }
            }

            //end("-> " + self);
            return self;
        }

        protected Set<SemanticContext> getFactoringOperands(SemanticContext context) {
            //begin(this.getOperatorString() + ".getFactoringOperands: " + context);

            Set<SemanticContext> result;
            if (context.getClass() == this.getClass()) {
                result = ((CommutativePredicate)context).operands;
            } else if (context instanceof NOT) {
                Set<SemanticContext> ops = this.inverse().getFactoringOperands(((NOT)context).ctx);
                result = new LinkedHashSet<SemanticContext>(ops.size());
                for (SemanticContext op : ops) {
                    result.add(not(op));
                }
            } else {
                result = new LinkedHashSet<SemanticContext>(1);
                result.add(context);
            }

            //end("-> " + result);
            return result;
        }

        protected SemanticContext simplify () {
            //begin(this.getOperatorString() + ".Simplifying: " + this);
            SemanticContext identity = this.getIdentityTerm();

            if (this.operands.remove(identity)) {
                //print("-- removed identity term: " + this);
            }

            if (this.operands.isEmpty()) {
                //end("-> empty: " + identity);
                return identity;
            }

            if (this.operands.size() == 1) {
                //print("Single element: " + this.operands);
                SemanticContext elem = this.operands.iterator().next();
                //end("-> " + elem);
                return elem;
            }

            SemanticContext absorb = this.getAbsorbingTerm();
            Set<SemanticContext> ops = new LinkedHashSet<SemanticContext>();
            Set<SemanticContext> notOps = new LinkedHashSet<SemanticContext>();
            for (SemanticContext op : this.operands) {
                if (absorb.equals(op)) {
                    //end("-> found an absorb term: " + absorb);
                    return absorb;
                }
                if (op instanceof NOT) {
                    SemanticContext notOp = ((NOT)op).ctx;
                    if (this.operands.contains(notOp)) {
                        // If we consider that this operand is !p, we just found
                        // that p is also in the operands. By definition,
                        // p op !p == absorb so this whole expression simplifies
                        // to absorb.
                        //end("-> found p, !p = " + absorb);
                        return absorb;
                    }
                    notOps.add(((NOT)op).ctx);
                } else {
                    ops.add(op);
                }
            }

            if (notOps.size() > ops.size()) {
                //print("Worth pulling up !");
                // NOT the ops and add to notOps
                for (SemanticContext op : ops) {
                    notOps.add(not(op));
                }
                SemanticContext notted = new NOT(this.inverse().make(notOps));
                //end("-> Pulled up !: " + notted);
                return notted;
            }

            //end("-> simple enough, returning self");
            return this;
        }

        @Override
        protected SemanticContext _substitute (Set<SemanticContext> terms, SemanticContext replacement) {
            //begin(this.getOperatorString() + "._substitute: " + this + " (" + terms + ", " + replacement + ")");
            Set<SemanticContext> newTerms = new LinkedHashSet<SemanticContext>(this.operands.size());

            for (SemanticContext term : this.operands) {
                newTerms.add(term.substitute(terms, replacement));
            }

            for (SemanticContext term : terms) {
                if (term.getClass() == this.getClass()) {
                    CommutativePredicate commTerm = (CommutativePredicate)term;

                    if (newTerms.containsAll(commTerm.operands)) {
                        newTerms.removeAll(commTerm.operands);
                        newTerms.add(replacement);
                    }
                }
            }


            SemanticContext result = null;
            if (this.operands.equals(newTerms)) {
                //print("No change");
                result = this;
            } else {
                //print("Substituted...");
                result = this.make(newTerms);
            }

            //end("-> " + result);
            return result;
        }
    }


    public static class AND extends CommutativePredicate {
        protected AND () {}

        protected AND (SemanticContext ... ops) {
            super(ops);
        }

        protected AND (Collection<SemanticContext> contexts) {
            super(contexts);
        }

        @Override
        public ST genExpr(CodeGenerator generator,
                          STGroup templates,
                          DFA dfa)
        {
            ST result = null;
            for (SemanticContext operand : operands) {
                if (result == null) {
                    result = operand.genExpr(generator, templates, dfa);
                    continue;
                }

                ST eST;
                if ( templates!=null ) {
                    eST = templates.getInstanceOf("andPredicates");
                }
                else {
                    eST = new ST("(<left>)&&(<right>)");
                }
                eST.add("left", result);
                eST.add("right", operand.genExpr(generator,templates,dfa));
                result = eST;
            }

            return result;
        }

        @Override
        public String getOperatorString() {
            return "&&";
        }

        @Override
        public int calculateHashCode() {
            int hashcode = 0;
            for (SemanticContext context : operands) {
                hashcode = hashcode ^ context.hashCode();
            }

            return hashcode;
        }

        @Override
        protected SemanticContext getAbsorbingTerm () {
            return new FalsePredicate();
        }

        @Override
        protected SemanticContext getIdentityTerm () {
            return new TruePredicate();
        }

        @Override
        protected CommutativePredicate inverse () {
            return SemanticContext.iOr;
        }

        @Override
        protected SemanticContext make (SemanticContext a, SemanticContext b) {
            //print("Making " + a + " && " + b);
            return new AND(a, b).simplify();
        }

        protected SemanticContext make (Collection<SemanticContext> ops) {
            //print("Making && " + ops);
            return new AND(ops).simplify();
        }
    }


    public static class OR extends CommutativePredicate {
        protected OR () {}

        protected OR (SemanticContext ... ops) {
            super(ops);
        }

        protected OR (Collection<SemanticContext> contexts) {
            super(contexts);
        }

        @Override
        public ST genExpr(CodeGenerator generator,
                          STGroup templates,
                          DFA dfa)
        {
            ST eST;
            if ( templates!=null ) {
                eST = templates.getInstanceOf("orPredicates");
            }
            else {
                eST = new ST("(<first(operands)>)<rest(operands):{o | ||(<o>)}>");
            }
            for (SemanticContext semctx : operands) {
                eST.add("operands", semctx.genExpr(generator,templates,dfa));
            }
            return eST;
        }

        @Override
        public String getOperatorString() {
            return "||";
        }

        @Override
        public int calculateHashCode() {
            int hashcode = 0;
            for (SemanticContext context : operands) {
                hashcode = ~hashcode ^ context.hashCode();
            }

            return hashcode;
        }

        @Override
        protected SemanticContext getAbsorbingTerm () {
            return new TruePredicate();
        }

        @Override
        protected SemanticContext getIdentityTerm () {
            return new FalsePredicate();
        }

        @Override
        protected CommutativePredicate inverse () {
            return SemanticContext.iAnd;
        }

        @Override
        protected SemanticContext make (SemanticContext a, SemanticContext b) {
            //print("Making " + a + " || " + b);
            return new OR(a, b).simplify();
        }

        @Override
        protected SemanticContext make (Collection<SemanticContext> ops) {
            //print("Making || " + ops);
            return new OR(ops).simplify();
        }

    }


    protected static AND iAnd = new AND();
    protected static OR iOr = new OR();

    public static SemanticContext and (SemanticContext ... ops) {
        return new AND(ops).simplify().factored();
    }

    public static SemanticContext or (SemanticContext ... ops) {
        return new OR(ops).simplify().factored();
    }

    public static SemanticContext not (SemanticContext a) {
        return new NOT(a).simplify();
    }

    /*
    protected static int depth = 0;
    public static void begin (String msg) {
        print(msg);
        depth++;
    }

    public static void end (String msg) {
        depth--;
        print(msg);
    }

    public static void print (String msg) {
        StringBuilder buf = new StringBuilder();
        for (int i = 0; i < depth; i++) {
            buf.append("  ");
        }
        buf.append(msg);

        //System.out.println(buf.toString());
    }
    */
}
