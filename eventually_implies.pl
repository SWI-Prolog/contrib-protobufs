
% %	~>(:P, :Q) is semidet.
% %     eventually_implies(P, Q) is semidet.
%    asserts temporal Liveness (something good happens, eventually) and
%    Safety (nothing bad ever happens) properties. Analogous to the
%    "leads-to" operator of Owicki and Lamport, 1982. Provides a sort of
%    lazy implication described informally as:
%
%    * Liveness: For all possible outcomes, P -> Q, eventually.
%    * Safety: For all possible outcomes, (\+P ; Q), is invariant.
%
%  Described practically:
%
%    P ~> Q, declares that if P is true, then Q must be true, now or at
%    some point in the future.
%

:- meta_predicate ~>(0,0).
:- op(950, xfy, ~>).

~>(P, Q) :-
	setup_call_cleanup(P,
			   (true; fail),
			   Q -> true; throw(error(goal_failed(Q), context(~>, _)))).

