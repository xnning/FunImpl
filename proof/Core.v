Require Export SfLib.

(* Terms *)

Inductive tm : Type :=
| var : id -> tm
| star : tm
| app : tm -> tm -> tm
| abs : id -> tm -> tm -> tm
| pi : id -> tm -> tm -> tm
| castu : tm -> tm -> tm
| castd : tm -> tm
| mu : id -> tm -> tm -> tm.

Tactic Notation "t_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "Variable" | Case_aux c "TypeofTypes"
    | Case_aux c "Application" | Case_aux c "Abstraction"
    | Case_aux c "Dependent Product" | Case_aux c "Cast Up"
    | Case_aux c "Cast Down" | Case_aux c "Polymorphic Recursion"].

Definition x := (Id 0).
Definition y := (Id 1).
Definition z := (Id 2).
Hint Unfold x.
Hint Unfold y.
Hint Unfold z.

(* Values *)

Inductive value : tm -> Prop :=
| v_star : value (star)
| v_abs : forall x t e, value (abs x t e)
| v_pi : forall x t t', value (pi x t t')
| v_castu : forall t e, value (castu t e).

Hint Constructors value.


(* Substition *)
(* Only consider simple situations, where we only substitute closed terms  *)

Reserved Notation "'[' x ':=' s ']' t" (at level 20).


Fixpoint subst (x:id) (s:tm) (t:tm) : tm :=
  match t with
  | var x' => if eq_id_dec x x' then s else t
  | star => star
  | app t1 t2 =>
      app ([x:=s]t1) ([x:=s]t2)
  | abs x' T t1 =>
      abs x' ([x:=s]T) (if eq_id_dec x x' then t1 else ([x:=s]t1))
  | pi x' T t1 =>
      pi x' ([x:=s]T) (if eq_id_dec x x' then t1 else ([x:=s]t1))
  | castu t e => castu ([x:=s]t) ([x:=s]e)
  | castd e => castd ([x:=s]e)
  | mu x' T t1 =>
      mu x' ([x:=s]T) (if eq_id_dec x x' then t1 else ([x:=s]t1))
  end

where "'[' x ':=' s ']' t" := (subst x s t).

(* Contexts *)

Definition context := list (id * tm).

Definition subst_cxtx x s (Gamma : context) :=
  map (fun p => match p with
                | (m, n) => (m, subst x s n)
                end) Gamma.

Definition append_cxtx (Gamma1 Gamma2 : context) := Gamma2 ++ Gamma1.

(* Operational Semantics *)

Reserved Notation "t1 '=>' t2" (at level 40).

Inductive step : tm -> tm -> Prop :=
| S_Beta : forall x t e1 e2, app (abs x t e1) e2 => [x:=e2]e1
| S_CastDownUp : forall t e, castd (castu t e) => e
| S_App : forall e1 e1' e2, e1 => e1' -> app e1 e2 => app e1' e2
| S_CastDown : forall e e', e => e' -> castd e => castd e'
| S_Mu : forall x t e, mu x t e => [x:=(mu x t e)]e

where "t1 '=>' t2" := (step t1 t2).

Tactic Notation "step_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "S_Beta" | Case_aux c "S_CastDownUp"
  | Case_aux c "ST_App" | Case_aux c "S_CastDown"
  | Case_aux c "ST_Mu"].

Hint Constructors step.

Notation multistep := (multi step).
Notation "t1 '=>*' t2" := (multistep t1 t2) (at level 40).

(* Typing *)

Definition ext (Gamma : context) (x:id) (T : tm) :=
  (x, T) :: Gamma.

Definition emp := nil : context.

Reserved Notation "Gamma '|-' t '\in' T" (at level 40).

Inductive has_type : context -> tm -> tm -> Prop :=
| T_Ax : forall Gamma, Gamma |- star \in star
| T_Var : forall Gamma x t,
    Gamma |- t \in star ->
    ext Gamma x t |- (var x) \in t
| T_Weak : forall Gamma x y t1 t2,
    y <> x ->
    Gamma |- (var x) \in t1 ->
    Gamma |- t2 \in star ->
    ext Gamma y t2 |- (var x) \in t1
| T_App : forall Gamma x t1 t2 e1 e2,
    Gamma |- e1 \in pi x t2 t1 ->
    Gamma |- e2 \in t2 ->
    Gamma |- app e1 e2 \in [x:=e2]t1
| T_Lam : forall Gamma x t1 t2 e,
    ext Gamma x t1 |- e \in t2 ->
    Gamma |- pi x t1 t2 \in star ->
    Gamma |- abs x t1 e \in pi x t1 t2
| T_Pi : forall Gamma x t1 t2,
    Gamma |- t1 \in star ->
    ext Gamma x t1 |- t2 \in star ->
    Gamma |- pi x t1 t2 \in star
| T_CastUp : forall Gamma t1 t2 e,
    Gamma |- e \in t2 ->
    Gamma |- t1 \in star ->
    t1 => t2 ->
    Gamma |- castu t1 e \in t1
| T_CastDown : forall Gamma e t1 t2,
    Gamma |- e \in t1 ->
    Gamma |- t2 \in star ->
    t1 => t2 ->
    Gamma |- castd e \in t2
| T_Mu : forall Gamma x t e,
    ext Gamma x t |- e \in t ->
    Gamma |- t \in star ->
    Gamma |- mu x t e \in t

where "Gamma '|-' t '\in' T" := (has_type Gamma t T).

Tactic Notation "has_type_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "T_Ax" | Case_aux c "T_Var" | Case_aux c "T_Weak"
  | Case_aux c "T_App" | Case_aux c "T_Lam"
  | Case_aux c "T_Pi" | Case_aux c "T_CastUp"
  | Case_aux c "T_CastDown" | Case_aux c "T_Mu"].

Hint Constructors has_type.

Example typing_eg :
  ext emp y star |- app (abs x star (var x)) (var y) \in star.
Proof. apply (T_App _ x star star _ _); eauto. Qed.

(* Properties *)

Lemma canonical_form_lam : forall e y t1 t2,
  emp |- e \in (pi y t1 t2) ->
  value e ->
  exists x u, e = abs x t1 u.
Proof with eauto. intros. inversion H0; subst; try inversion H; subst...
  Case "CastUp" . inversion H; subst. inversion H7...
Qed.

Lemma canonical_form_castd : forall t e,
   emp |- castd e \in t ->
   value e ->
   exists t' e', e = castu t' e'.
Proof with eauto. intros. inversion H0; subst...
  Case "Star". inversion H; subst. inversion H2; subst. inversion H5.
  Case "Abs". inversion H; subst. inversion H2; subst. inversion H5.
  Case "Pi" . inversion H; subst. inversion H2; subst. inversion H5.
Qed.

(* Free variables *)

Inductive appears_free_in : id -> tm -> Prop :=
| afi_var : forall x, appears_free_in x (var x)
| afi_app1 : forall x e1 e2,
    appears_free_in x e1 -> appears_free_in x (app e1 e2)
| afi_app2 : forall x e1 e2,
    appears_free_in x e2 -> appears_free_in x (app e1 e2)
| afi_abs1 : forall x y t e,
    appears_free_in x t ->
    appears_free_in x (abs y t e)
| afi_abs2 : forall x y t e,
    y <> x ->
    appears_free_in x e ->
    appears_free_in x (abs y t e)
| afi_pi1 : forall x y t1 t2,
    appears_free_in x t1 ->
    appears_free_in x (pi y t1 t2)
| afi_pi2 : forall x y t1 t2,
    y <> x ->
    appears_free_in x t2 ->
    appears_free_in x (pi y t1 t2)
| afi_castu1 : forall x t e,
    appears_free_in x t ->
    appears_free_in x (castu t e)
| afi_castu2 : forall x t e,
    appears_free_in x e ->
    appears_free_in x (castu t e)
| afi_castd : forall x e,
    appears_free_in x e ->
    appears_free_in x (castd e)
| afi_mu1 : forall x y t e,
    appears_free_in x t ->
    appears_free_in x (mu y t e)
| afi_mu2 : forall x y t e,
    y <> x ->
    appears_free_in x e ->
    appears_free_in x (mu y t e).


Hint Constructors appears_free_in.

Definition closed (t : tm) :=
  forall x, ~ appears_free_in x t.

Lemma subst_not_free : forall x m n,
    ~ appears_free_in x m -> [x:=n]m = m.
Proof.
  intros. (* generalize dependent n. *)
  t_cases (induction m) Case; intros; auto; simpl.
  Case "Variable".
    destruct (eq_id_dec x0 i); subst. apply ex_falso_quodlibet. auto. auto.
  Case "Application".
    assert ([x0 := n]m1 = m1). auto.
    assert ([x0 := n]m2 = m2). auto.
    rewrite H0.
    rewrite H1.
    auto.
  Case "Abstraction".
    assert ([x0 := n]m1 = m1); auto; clear IHm1.
    destruct (eq_id_dec x0 i). subst x0.
    SCase "x == i".
      rewrite H0.
      auto.
    SCase "x <> i".
      assert ([x0 := n]m2 = m2). auto; clear IHm2.
      rewrite H0.
      rewrite H1.
      auto.
  Case "Dependent Product".
    assert ([x0 := n]m1 = m1); auto; clear IHm1.
    destruct (eq_id_dec x0 i). subst x0.
    SCase "x == i".
      rewrite H0.
      auto.
    SCase "x <> i".
      assert ([x0 := n]m2 = m2). auto; clear IHm2.
      rewrite H0.
      rewrite H1.
      auto.
  Case "Cast Up".
    simpl.
    assert ([x0 := n]m1 = m1).
    apply IHm1. auto.
    assert ([x0 := n]m2 = m2).
    apply IHm2. auto.
    rewrite H0.
    rewrite H1.
    auto.
  Case "Cast Down".
    simpl.
    assert ([x0 := n]m = m).
    apply IHm. auto.
    rewrite H0. auto.
  Case "Polymorphic Recursion".
    assert ([x0 := n]m1 = m1); auto; clear IHm1.
    destruct (eq_id_dec x0 i). subst x0.
    SCase "x == i".
      rewrite H0.
      auto.
    SCase "x <> i".
      assert ([x0 := n]m2 = m2). auto; clear IHm2.
      rewrite H0.
      rewrite H1.
      auto.
Qed.

(* Lemma subst_double : forall x y m n l, *)
(*     x <> y -> ~ appears_free_in x l -> *)
(*     [y:=l]([x:=n]m) = [x:=([y:=l]n)]([y:=l]m). *)
(* Proof.  Admitted. *)
(*   (* intros. *) *)
(*   (* t_cases (induction m) Case; auto. *) *)
(*   (* Case "Variable". *) *)
(*   (*   simpl. destruct (eq_id_dec x i); subst. rewrite neq_id. simpl. rewrite eq_id. auto. *) *)
(*   (*   auto. destruct (eq_id_dec y i); subst. simpl. rewrite eq_id. rewrite subst_not_free; auto. simpl. rewrite neq_id. rewrite neq_id. auto. auto. auto. *) *)
(*   (* Case "Application". *) *)
(*   (*   simpl. rewrite IHm1. rewrite IHm2. auto. *) *)
(*   (* Case "Abstraction". *) *)
(*   (*   simpl. destruct (eq_id_dec y i); subst. destruct (eq_id_dec x i); subst. apply ex_falso_quodlibet. apply H. auto. rewrite IHm1.  *) *)


(* Note that contexts are ordered lists and legal*)
Lemma well_typed_has_no_free_var : forall Gamma x t,
    Gamma |- (var x) \in t -> ~ appears_free_in x t.
Proof. Admitted.

Lemma subst_preserve_type : forall Gamma1 Gamma2 x e1 t1 e2 t2 ,
    append_cxtx (ext Gamma1 x t2) Gamma2 |- e1 \in t1 ->
    Gamma1 |- e2 \in t2 -> append_cxtx Gamma1 (subst_cxtx x e2 Gamma2) |- [x:=e2]e1 \in [x:=e2]t1.
Proof.
  intros.
  generalize dependent e2.
  t_cases (induction e1) Case.
  Case "Variable".
    intros e2 H2.
    destruct Gamma2.
    SCase "Gamma2 == <>".
      simpl in H.
      inversion H; subst.
      SSCase "T_Var".
        simpl.
        rewrite eq_id.
        apply well_typed_has_no_free_var in H.
        eapply subst_not_free in H.
        rewrite H.
        assumption.
      SSCase "T_Weak".
        simpl.
        rewrite neq_id.
Abort.


(* Progress *)

Theorem progress : forall e t,
    emp |- e \in t -> value e \/ exists e', e => e'.
Proof with eauto.
  intros e t H.
  remember emp as Gamma.
  has_type_cases (induction H) Case; subst; eauto; try (unfold ext in HeqGamma; unfold emp in HeqGamma; inversion HeqGamma).
  Case "T_App".
    right. destruct IHhas_type1...
    SCase "e1 is a value".
      assert (exists x u, e1 = abs x t2 u).
      eapply canonical_form_lam...
      destruct H2 as [y0 [y1 Heq]]. subst.
      exists ([y0:=e2]y1)...
    SCase "e1 steps".
      inversion H1; subst...
  Case "T_CastDown".
    right. destruct IHhas_type1...
    SCase "e is a value".
      assert (exists t' e', e = castu t' e').
      eapply canonical_form_castd...
      destruct H3 as [t' [e' Heq]]. subst.
      exists e'...
    SCase "e steps".
      inversion H2; subst...
Qed.

(* Decidability of Typechecking *)

Lemma step_decidability :
  forall e, {exists e', e => e'} + {~ exists e', e => e'}.
Proof.
  intros.
  t_cases (induction e) Case.
  Case "Variable".
    right. intros contra. inversion contra. inversion H.
  Case "TypeofTypes".
    right. intros contra. inversion contra. inversion H.
  Case "Application".
    inversion IHe1.
    left.
    inversion H.
    exists (app x0 e2).
    auto.
    destruct e1.
    try right.
    intros [e' H1].
    inversion H1; subst.
    inversion H4.
    right.
    intros [e' H1].
    inversion H1; subst.
    inversion H4.
    right.
    intros [e' H1].
    inversion H1; subst.
    apply H.
    exists e1'.
    auto.
    left.
    exists ([i:=e2]e1_2).
    auto.
    right.
    intros [e' H1].
    inversion H1; subst.
    inversion H4.
    right.
    intros [e' H1].
    inversion H1; subst.
    inversion H4.
    right.
    intros [e' H1].
    inversion H1; subst.
    apply H.
    exists e1'.
    auto.
    right.
    intros [e' H1].
    inversion H1; subst.
    apply H.
    exists e1'.
    auto.
  Case "Abstraction".
    right.
    intros [e' contra].
    inversion contra.
  Case "Dependent Product".
    right.
    intros [e' contra].
    inversion contra.
  Case "Cast Up".
    right.
    intros [e' contra].
    inversion contra.
  Case "Cast Down".
    inversion IHe.
    SCase "e => e'".
    left.
    inversion H.
    exists (castd x0).
    auto.
    destruct e.
    right.
    intros [e' contra].
    inversion contra; subst.
    inversion H1.
    right.
    intros [e' contra].
    inversion contra; subst.
    inversion H1.
    right.
    intros [e' contra].
    inversion contra; subst.
    apply H.
    exists e'0.
    auto.
    right.
    intros [e' contra].
    inversion contra; subst.
    inversion H1.
    right.
    intros [e' contra].
    inversion contra; subst.
    inversion H1.
    left.
    exists e2.
    auto.
    right.
    intros [e' contra].
    inversion contra; subst.
    apply H.
    exists e'0.
    auto.
    right.
    intros [e' contra].
    inversion contra; subst.
    apply H.
    exists e'0.
    auto.
  Case "Polymorphic Recursion".
    left.
    exists ([i:=mu i e1 e2] e2).
    auto.
Qed.

Lemma type_star :
  forall Gamma x t e t',
    ext Gamma x t |- e \in t' -> Gamma |- t \in star.
Proof. Admitted.

Theorem tc_decidability :
  forall Gamma e, {exists t, Gamma |- e \in t} + {~ exists t, Gamma |- e \in t}.
Proof.
  intros.
  t_cases (induction e) Case.
  destruct Gamma.
  SCase "Gamma is empty".
    right.
    intros H.
    inversion H.
    inversion H0.
  SCase "Gamma has one element".
    assert (p :: Gamma = ext Gamma (fst p) (snd p)).
    unfold ext.
    rewrite <- (surjective_pairing p).
    reflexivity.
    remember (fst p) as x.
    remember (snd p) as t.
    destruct (eq_id_dec x i).
    SSCase "x == i".
      left.
      exists t.
      rewrite H.
      subst i.
      apply T_Var.
Abort.
