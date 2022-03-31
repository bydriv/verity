use crate::common::*;

pub enum QualifiedIdentifier {
    Identifier {
        identifier: Lexeme,
    },
    Dot {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        identifier: Lexeme,
    },
}

pub enum AtomicKindExpression {
    Type {
        type_: Lexeme,
    },
    PositiveType {
        plus: Lexeme,
        type_: Lexeme,
    },
    NegativeType {
        minus: Lexeme,
        type_: Lexeme,
    },
    Prec {
        lparen: Lexeme,
        kind_expression: Constituent<KindExpression>,
        rparen: Lexeme,
    },
}

pub enum MultiplicativeKindExpression {
    AtomicKindExpression {
        atomic_kind_expression: Constituent<AtomicKindExpression>,
    },
    Asterisk {
        atomic_kind_expression: Constituent<AtomicKindExpression>,
        asterisk: Lexeme,
        multiplicative_kind_expression: Constituent<MultiplicativeKindExpression>,
    },
}

pub enum KindExpression {
    MultiplicativeKindExpression {
        multiplicative_kind_expression: Constituent<MultiplicativeKindExpression>,
    },
    Arrow {
        multiplicative_kind_expression: Constituent<MultiplicativeKindExpression>,
        arrow: Lexeme,
        kind_expression: Constituent<KindExpression>,
    },
}

pub enum AtomicTypeParameter {
    Identifier {
        identifier: Lexeme,
    },
    Tuple {
        lparen: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rparen: Lexeme,
    },
}

pub enum TypeParameter {
    AtomicTypeParameter {
        atomic_type_parameter: Constituent<AtomicTypeParameter>,
    },
    Colon {
        identifier: Lexeme,
        colon: Lexeme,
        kind_expression: Constituent<KindExpression>,
    },
}

pub enum TypeParameters {
    One {
        type_parameter: Constituent<TypeParameter>,
    },
    More {
        type_parameter: Constituent<TypeParameter>,
        comma: Lexeme,
        type_parameters: Constituent<TypeParameters>,
    },
}

pub enum TypeMember {
    Required {
        identifier: Lexeme,
        colon: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
    Optional {
        identifier: Lexeme,
        question: Lexeme,
        colon: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
}

pub enum TypeMembers {
    One {
        type_member: Constituent<TypeMember>,
    },
    More {
        type_member: Constituent<TypeMember>,
        comma: Lexeme,
        type_members: Constituent<TypeMembers>,
    },
}

pub enum RecordExpression {
    Empty {
        lbrace: Lexeme,
        rbrace: Lexeme,
    },
    NonEmpty {
        lbrace: Lexeme,
        type_members: Constituent<TypeMembers>,
        rbrace: Lexeme,
    },
}

pub enum TypeVariant {
    Identifier {
        identifier: Lexeme,
    },
    Of {
        identifier: Lexeme,
        of: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
}

pub enum TypeVariants {
    One {
        type_variant: Constituent<TypeVariant>,
    },
    More {
        type_variant: Constituent<TypeVariant>,
        pipe: Lexeme,
        type_variants: Constituent<TypeVariants>,
    },
}

pub enum VariantExpression {
    Empty {
        lbracket: Lexeme,
        rbracket: Lexeme,
    },
    NonEmpty {
        lbracket: Lexeme,
        type_variants: Constituent<TypeVariants>,
        rbracket: Lexeme,
    },
}

pub enum StructureExpression {
    RecordExpression {
        record_expression: Constituent<RecordExpression>,
    },
    VariantExpression {
        variant_expression: Constituent<VariantExpression>,
    },
}

pub enum AtomicTypeExpression {
    QualifiedIdentifier {
        qualified_identifier: Constituent<QualifiedIdentifier>,
    },
    Tuple {
        lparen: Lexeme,
        type_expressions: Constituent<TypeExpressions>,
        rparen: Lexeme,
    },
}

pub enum ApplicativeTypeExpression {
    AtomicTypeExpression {
        atomic_type_expression: Constituent<AtomicTypeExpression>,
    },
    Apply {
        applicative_type_expression: Constituent<ApplicativeTypeExpression>,
        atomic_type_expression: Constituent<AtomicTypeExpression>,
    },
}

pub enum ReferenceTypeExpression {
    ApplicativeTypeExpression {
        applicative_type_expression: Constituent<ApplicativeTypeExpression>,
    },
    Amp {
        amp: Lexeme,
        reference_type_expression: Constituent<ReferenceTypeExpression>,
    },
}

pub enum EffectiveTypeExpression {
    ReferenceTypeExpression {
        reference_type_expression: Constituent<ReferenceTypeExpression>,
    },
    Exclamation {
        effective_type_expression: Constituent<EffectiveTypeExpression>,
        exclamation: Lexeme,
    },
}

pub enum MultiplicativeTypeExpression {
    EffectiveTypeExpression {
        effective_type_expression: Constituent<EffectiveTypeExpression>,
    },
    Asterisk {
        effective_type_expression: Constituent<EffectiveTypeExpression>,
        asterisk: Lexeme,
        multiplicative_type_expression: Constituent<MultiplicativeTypeExpression>,
    },
}

pub enum TypeExpression {
    MultiplicativeTypeExpression {
        multiplicative_type_expression: Constituent<MultiplicativeTypeExpression>,
    },
    Arrow {
        multiplicative_type_expression: Constituent<MultiplicativeTypeExpression>,
        arrow: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
    Abstraction {
        hat: Lexeme,
        atomic_type_parameter: Constituent<AtomicTypeParameter>,
        dot: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
    Forall {
        forall: Lexeme,
        atomic_type_parameter: Constituent<AtomicTypeParameter>,
        dot: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
    Exists {
        exists: Lexeme,
        atomic_type_parameter: Constituent<AtomicTypeParameter>,
        dot: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
}

pub enum TypeExpressions {
    One {
        type_expression: Constituent<TypeExpression>,
    },
    More {
        type_expression: Constituent<TypeExpression>,
        comma: Lexeme,
        type_expressions: Constituent<TypeExpressions>,
    },
}

pub enum TypeDefinition {
    Type {
        type_: Lexeme,
        identifier: Lexeme,
        coloneq: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
    TypeColon {
        type_: Lexeme,
        identifier: Lexeme,
        colon: Lexeme,
        kind_expression: Constituent<KindExpression>,
        coloneq: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
    TypeFn {
        type_: Lexeme,
        identifier: Lexeme,
        lparen: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rparen: Lexeme,
        coloneq: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
    TypeFnColon {
        type_: Lexeme,
        identifier: Lexeme,
        lparen: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rparen: Lexeme,
        colon: Lexeme,
        kind_expression: Constituent<KindExpression>,
        coloneq: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
    Struct {
        struct_: Lexeme,
        identifier: Lexeme,
        coloneq: Lexeme,
        structure_expression: Constituent<StructureExpression>,
    },
    StructColon {
        struct_: Lexeme,
        identifier: Lexeme,
        colon: Lexeme,
        kind_expression: Constituent<KindExpression>,
        coloneq: Lexeme,
        structure_expression: Constituent<StructureExpression>,
    },
    StructFn {
        struct_: Lexeme,
        identifier: Lexeme,
        lparen: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rparen: Lexeme,
        coloneq: Lexeme,
        structure_expression: Constituent<StructureExpression>,
    },
    StructFnColon {
        struct_: Lexeme,
        identifier: Lexeme,
        lparen: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rparen: Lexeme,
        colon: Lexeme,
        kind_expression: Constituent<KindExpression>,
        coloneq: Lexeme,
        structure_expression: Constituent<StructureExpression>,
    },
    Inductive {
        inductive: Lexeme,
        identifier: Lexeme,
        coloneq: Lexeme,
        structure_expression: Constituent<StructureExpression>,
    },
    InductiveColon {
        inductive: Lexeme,
        identifier: Lexeme,
        colon: Lexeme,
        kind_expression: Constituent<KindExpression>,
        coloneq: Lexeme,
        structure_expression: Constituent<StructureExpression>,
    },
    InductiveFn {
        inductive: Lexeme,
        identifier: Lexeme,
        lparen: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rparen: Lexeme,
        coloneq: Lexeme,
        structure_expression: Constituent<StructureExpression>,
    },
    InductiveFnColon {
        inductive: Lexeme,
        identifier: Lexeme,
        lparen: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rparen: Lexeme,
        colon: Lexeme,
        kind_expression: Constituent<KindExpression>,
        coloneq: Lexeme,
        structure_expression: Constituent<StructureExpression>,
    },
}

pub enum ValueParameterMember {
    Identifier {
        identifier: Lexeme,
    },
    Default {
        identifier: Lexeme,
        question: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefaultMap {
        identifier: Lexeme,
        question: Lexeme,
        value_expression1: Constituent<ValueExpression>,
        colon: Lexeme,
        value_expression2: Constituent<ValueExpression>,
    },
    Colon {
        identifier: Lexeme,
        colon: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
    ColonDefault {
        identifier: Lexeme,
        colon: Lexeme,
        type_expression: Constituent<TypeExpression>,
        question: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    ColonDefaultMap {
        identifier: Lexeme,
        colon1: Lexeme,
        type_expression: Constituent<TypeExpression>,
        question: Lexeme,
        value_expression1: Constituent<ValueExpression>,
        colon2: Lexeme,
        value_expression2: Constituent<ValueExpression>,
    },
}

pub enum ValueParameterMembers {
    One {
        value_parameter_member: Constituent<ValueParameterMember>,
    },
    More {
        value_parameter_member: Constituent<ValueParameterMember>,
        comma: Lexeme,
        value_parameter_members: Constituent<ValueParameterMembers>,
    },
}

pub enum AtomicValueParameter {
    Identifier {
        identifier: Lexeme,
    },
    Tuple {
        lparen: Lexeme,
        value_parameters: Constituent<ValueParameters>,
        rparen: Lexeme,
    },
}

pub enum ValueParameter {
    AtomicValueParameter {
        atomic_value_parameter: Constituent<AtomicValueParameter>,
    },
    Colon {
        identifier: Lexeme,
        colon: Lexeme,
        type_expression: Constituent<TypeExpression>,
    },
    EmptyRecord {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        lbrace: Lexeme,
        rbrace: Lexeme,
    },
    NonEmptyRecord {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        lbrace: Lexeme,
        value_parameter_members: Constituent<ValueParameterMembers>,
        rbrace: Lexeme,
    },
    Family {
        family: Lexeme,
        qualified_identifier: Constituent<QualifiedIdentifier>,
        atomic_value_parameter: Constituent<AtomicValueParameter>,
    },
}

pub enum ValueParameters {
    One {
        value_parameter: Constituent<ValueParameter>,
    },
    More {
        value_parameter: Constituent<ValueParameter>,
        comma: Lexeme,
        value_parameters: Constituent<ValueParameters>,
    },
}

pub enum ValueMember {
    Identifier {
        identifier: Lexeme,
    },
    Coloneq {
        identifier: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
}

pub enum ValueMembers {
    One {
        value_member: Constituent<ValueMember>,
    },
    More {
        value_member: Constituent<ValueMember>,
        comma: Lexeme,
        value_members: Constituent<ValueMembers>,
    },
}

pub enum ValueImplication {
    Variant {
        identifier: Lexeme,
        darrow: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    VariantOf {
        identifier: Lexeme,
        atomic_value_parameter: Constituent<AtomicValueParameter>,
        darrow: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
}

pub enum ValueImplications {
    One {
        value_implication: Constituent<ValueImplication>,
    },
    More {
        value_implication: Constituent<ValueImplication>,
        pipe: Lexeme,
        value_implications: Constituent<ValueImplications>,
    },
}

pub enum AtomicValueExpression {
    False {
        false_: Lexeme,
    },
    True {
        true_: Lexeme,
    },
    Integer {
        integer: Lexeme,
    },
    Rational {
        rational: Lexeme,
    },
    String {
        string: Lexeme,
    },
    Binary {
        binary: Lexeme,
    },
    QualifiedIdentifier {
        qualified_identifier: Constituent<QualifiedIdentifier>,
    },
    QualifiedIdentifierDotEq {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        eq: Lexeme,
    },
    QualifiedIdentifierDotNoteq {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        noteq: Lexeme,
    },
    QualifiedIdentifierDotLte {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        lte: Lexeme,
    },
    QualifiedIdentifierDotLt {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        lt: Lexeme,
    },
    QualifiedIdentifierDotGte {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        gte: Lexeme,
    },
    QualifiedIdentifierDotGt {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        gt: Lexeme,
    },
    QualifiedIdentifierDotPlus {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        plus: Lexeme,
    },
    QualifiedIdentifierDotMinus {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        minus: Lexeme,
    },
    QualifiedIdentifierDotAsterisk {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        asterisk: Lexeme,
    },
    QualifiedIdentifierDotSlash {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        slash: Lexeme,
    },
    QualifiedIdentifierDotPercent {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        percent: Lexeme,
    },
    QualifiedIdentifierColoncolonIdentifier {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        coloncolon: Lexeme,
        identifier: Lexeme,
    },
    EmptyArray {
        lbracket: Lexeme,
        rbracket: Lexeme,
    },
    NonEmptyArray {
        lbracket: Lexeme,
        value_expressions: Constituent<ValueExpressions>,
        rbracket: Lexeme,
    },
    MapArray {
        lbracket: Lexeme,
        value_expression1: Constituent<ValueExpression>,
        pipe: Lexeme,
        value_parameter: Constituent<ValueParameter>,
        in_: Lexeme,
        value_expression2: Constituent<ValueExpression>,
        rbracket: Lexeme,
    },
    MapRange {
        lbracket: Lexeme,
        value_expression1: Constituent<ValueExpression>,
        pipe: Lexeme,
        value_parameter: Constituent<ValueParameter>,
        in_: Lexeme,
        value_expression2: Constituent<ValueExpression>,
        dotdot: Lexeme,
        value_expression3: Constituent<ValueExpression>,
        rbracket: Lexeme,
    },
    Tuple {
        lparen: Lexeme,
        value_expressions: Constituent<ValueExpressions>,
        rparen: Lexeme,
    },
}

pub enum ApplicativeValueExpression {
    AtomicValueExpression {
        atomic_value_expression: Constituent<AtomicValueExpression>,
    },
    Apply {
        applicative_value_expression: Constituent<ApplicativeValueExpression>,
        atomic_value_expression: Constituent<AtomicValueExpression>,
    },
}

pub enum RecordValueExpression {
    ApplicativeValueExpression {
        applicative_value_expression: Constituent<ApplicativeValueExpression>,
    },
    EmptyRecord {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        lbrace: Lexeme,
        rbrace: Lexeme,
    },
    NonEmptyRecord {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        lbrace: Lexeme,
        value_members: Constituent<ValueMembers>,
        rbrace: Lexeme,
    },
}

pub enum ComplementaryValueExpression {
    RecordValueExpression {
        record_value_expression: Constituent<RecordValueExpression>,
    },
    Exclamation {
        exclamation: Lexeme,
        complementary_value_expression: Constituent<ComplementaryValueExpression>,
    },
    Amp {
        amp: Lexeme,
        complementary_value_expression: Constituent<ComplementaryValueExpression>,
    },
    Asterisk {
        asterisk: Lexeme,
        complementary_value_expression: Constituent<ComplementaryValueExpression>,
    },
}

pub enum MultiplicativeValueExpression {
    ComplementaryValueExpression {
        complementary_value_expression: Constituent<ComplementaryValueExpression>,
    },
    Asterisk {
        multiplicative_value_expression: Constituent<MultiplicativeValueExpression>,
        asterisk: Lexeme,
        complementary_value_expression: Constituent<ComplementaryValueExpression>,
    },
    Slash {
        multiplicative_value_expression: Constituent<MultiplicativeValueExpression>,
        slash: Lexeme,
        complementary_value_expression: Constituent<ComplementaryValueExpression>,
    },
    Percent {
        multiplicative_value_expression: Constituent<MultiplicativeValueExpression>,
        percent: Lexeme,
        complementary_value_expression: Constituent<ComplementaryValueExpression>,
    },
}

pub enum AdditiveValueExpression {
    MultiplicativeValueExpression {
        multiplicative_value_expression: Constituent<MultiplicativeValueExpression>,
    },
    Plus {
        additive_value_expression: Constituent<AdditiveValueExpression>,
        plus: Lexeme,
        multiplicative_value_expression: Constituent<MultiplicativeValueExpression>,
    },
    Minus {
        additive_value_expression: Constituent<AdditiveValueExpression>,
        minus: Lexeme,
        multiplicative_value_expression: Constituent<MultiplicativeValueExpression>,
    },
}

pub enum ComparativeValueExpression {
    AdditiveValueExpression {
        additive_value_expression: Constituent<AdditiveValueExpression>,
    },
    Eq {
        comparative_value_expression: Constituent<ComparativeValueExpression>,
        eq: Lexeme,
        additive_value_expression: Constituent<AdditiveValueExpression>,
    },
    Noteq {
        comparative_value_expression: Constituent<ComparativeValueExpression>,
        noteq: Lexeme,
        additive_value_expression: Constituent<AdditiveValueExpression>,
    },
    Lte {
        comparative_value_expression: Constituent<ComparativeValueExpression>,
        lte: Lexeme,
        additive_value_expression: Constituent<AdditiveValueExpression>,
    },
    Lt {
        comparative_value_expression: Constituent<ComparativeValueExpression>,
        lt: Lexeme,
        additive_value_expression: Constituent<AdditiveValueExpression>,
    },
    Gte {
        comparative_value_expression: Constituent<ComparativeValueExpression>,
        gte: Lexeme,
        additive_value_expression: Constituent<AdditiveValueExpression>,
    },
    Gt {
        comparative_value_expression: Constituent<ComparativeValueExpression>,
        gt: Lexeme,
        additive_value_expression: Constituent<AdditiveValueExpression>,
    },
}

pub enum ConjunctiveValueExpression {
    ComparativeValueExpression {
        comparative_value_expression: Constituent<ComparativeValueExpression>,
    },
    And {
        conjunctive_value_expression: Constituent<ConjunctiveValueExpression>,
        and: Lexeme,
        comparative_value_expression: Constituent<ComparativeValueExpression>,
    },
}

pub enum DisjunctiveValueExpression {
    ConjunctiveValueExpression {
        conjunctive_value_expression: Constituent<ConjunctiveValueExpression>,
    },
    Or {
        disjunctive_value_expression: Constituent<DisjunctiveValueExpression>,
        or: Lexeme,
        conjunctive_value_expression: Constituent<ConjunctiveValueExpression>,
    },
}

pub enum AssignmentValueExpression {
    DisjunctiveValueExpression {
        disjunctive_value_expression: Constituent<DisjunctiveValueExpression>,
    },
    Coloneq {
        disjunctive_value_expression: Constituent<DisjunctiveValueExpression>,
        coloneq: Lexeme,
        assignment_value_expression: Constituent<AssignmentValueExpression>,
    },
}

pub enum ControlValueExpression {
    AssignmentValueExpression {
        assignment_value_expression: Constituent<AssignmentValueExpression>,
    },
    EmptyMatch {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        match_: Lexeme,
        value_expression: Constituent<ValueExpression>,
        with: Lexeme,
        end: Lexeme,
    },
    NonEmptyMatch {
        qualified_identifier: Constituent<QualifiedIdentifier>,
        dot: Lexeme,
        match_: Lexeme,
        value_expression: Constituent<ValueExpression>,
        with: Lexeme,
        value_implications: Constituent<ValueImplications>,
        end: Lexeme,
    },
    ForArray {
        for_: Lexeme,
        value_parameter: Constituent<ValueParameter>,
        in_: Lexeme,
        value_expression1: Constituent<ValueExpression>,
        do_: Lexeme,
        value_expression2: Constituent<ValueExpression>,
        end: Lexeme,
    },
    ForRange {
        for_: Lexeme,
        value_parameter: Constituent<ValueParameter>,
        in_: Lexeme,
        value_expression1: Constituent<ValueExpression>,
        dotdot: Lexeme,
        value_expression2: Constituent<ValueExpression>,
        do_: Lexeme,
        value_expression3: Constituent<ValueExpression>,
        end: Lexeme,
    },
}

pub enum ValueExpression {
    ControlValueExpression {
        control_value_expression: Constituent<ControlValueExpression>,
    },
    Semicolon {
        control_value_expression: Constituent<ControlValueExpression>,
        semicolon: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    Abstraction {
        hat: Lexeme,
        atomic_value_parameter: Constituent<AtomicValueParameter>,
        dot: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    Let {
        let_: Lexeme,
        identifier: Lexeme,
        coloneq: Lexeme,
        value_expression1: Constituent<ValueExpression>,
        in_: Lexeme,
        value_expression2: Constituent<ValueExpression>,
    },
    If {
        if_: Lexeme,
        value_expression1: Constituent<ValueExpression>,
        then: Lexeme,
        value_expression2: Constituent<ValueExpression>,
        else_: Lexeme,
        value_expression3: Constituent<ValueExpression>,
    },
}

pub enum ValueExpressions {
    One {
        value_expression: Constituent<ValueExpression>,
    },
    More {
        value_expression: Constituent<ValueExpression>,
        comma: Lexeme,
        value_expressions: Constituent<ValueExpressions>,
    },
}

pub enum ValueDefinition {
    Def {
        def: Lexeme,
        identifier: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefColon {
        def: Lexeme,
        identifier: Lexeme,
        colon: Lexeme,
        type_expression: Constituent<TypeExpression>,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefFn {
        def: Lexeme,
        identifier: Lexeme,
        lparen: Lexeme,
        value_parameters: Constituent<ValueParameters>,
        rparen: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefFnColon {
        def: Lexeme,
        identifier: Lexeme,
        lparen: Lexeme,
        value_parameters: Constituent<ValueParameters>,
        rparen: Lexeme,
        colon: Lexeme,
        type_expression: Constituent<TypeExpression>,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefPoly {
        def: Lexeme,
        identifier: Lexeme,
        lbracket: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rbracket: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefPolyColon {
        def: Lexeme,
        identifier: Lexeme,
        lbracket: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rbracket: Lexeme,
        colon: Lexeme,
        type_expression: Constituent<TypeExpression>,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefPolyFn {
        def: Lexeme,
        identifier: Lexeme,
        lbracket: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rbracket: Lexeme,
        lparen: Lexeme,
        value_parameters: Constituent<ValueParameters>,
        rparen: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefPolyFnColon {
        def: Lexeme,
        identifier: Lexeme,
        lbracket: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rbracket: Lexeme,
        lparen: Lexeme,
        value_parameters: Constituent<ValueParameters>,
        rparen: Lexeme,
        colon: Lexeme,
        type_expression: Constituent<TypeExpression>,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefEq {
        def: Lexeme,
        identifier1: Lexeme,
        eq: Lexeme,
        identifier2: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefNoteq {
        def: Lexeme,
        identifier1: Lexeme,
        noteq: Lexeme,
        identifier2: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefLte {
        def: Lexeme,
        identifier1: Lexeme,
        lte: Lexeme,
        identifier2: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefLt {
        def: Lexeme,
        identifier1: Lexeme,
        lt: Lexeme,
        identifier2: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefGte {
        def: Lexeme,
        identifier1: Lexeme,
        gte: Lexeme,
        identifier2: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefGt {
        def: Lexeme,
        identifier1: Lexeme,
        gt: Lexeme,
        identifier2: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefPlus {
        def: Lexeme,
        identifier1: Lexeme,
        plus: Lexeme,
        identifier2: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefMinus {
        def: Lexeme,
        identifier1: Lexeme,
        minus: Lexeme,
        identifier2: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefAsterisk {
        def: Lexeme,
        identifier1: Lexeme,
        asterisk: Lexeme,
        identifier2: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefSlash {
        def: Lexeme,
        identifier1: Lexeme,
        slash: Lexeme,
        identifier2: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
    DefPercent {
        def: Lexeme,
        identifier1: Lexeme,
        percent: Lexeme,
        identifier2: Lexeme,
        coloneq: Lexeme,
        value_expression: Constituent<ValueExpression>,
    },
}

pub enum FamilyExpression {
    Family {
        family: Lexeme,
        definitions: Constituent<Definitions>,
        end: Lexeme,
    },
}

pub enum FamilyDefinition {
    Family {
        family: Lexeme,
        identifier: Lexeme,
        coloneq: Lexeme,
        family_expression: Constituent<FamilyExpression>,
    },
    FamilySubtype {
        family: Lexeme,
        identifier: Lexeme,
        subtype: Lexeme,
        type_expression: Constituent<TypeExpression>,
        coloneq: Lexeme,
        family_expression: Constituent<FamilyExpression>,
    },
    FamilyFn {
        family: Lexeme,
        identifier: Lexeme,
        lparen: Lexeme,
        value_parameters: Constituent<ValueParameters>,
        rparen: Lexeme,
        coloneq: Lexeme,
        family_expression: Constituent<FamilyExpression>,
    },
    FamilyFnSubtype {
        family: Lexeme,
        identifier: Lexeme,
        lparen: Lexeme,
        value_parameters: Constituent<ValueParameters>,
        rparen: Lexeme,
        subtype: Lexeme,
        type_expression: Constituent<TypeExpression>,
        coloneq: Lexeme,
        family_expression: Constituent<FamilyExpression>,
    },
    PolyFamily {
        family: Lexeme,
        identifier: Lexeme,
        lbracket: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rbracket: Lexeme,
        coloneq: Lexeme,
        family_expression: Constituent<FamilyExpression>,
    },
    PolyFamilySubtype {
        family: Lexeme,
        identifier: Lexeme,
        lbracket: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rbracket: Lexeme,
        subtype: Lexeme,
        type_expression: Constituent<TypeExpression>,
        coloneq: Lexeme,
        family_expression: Constituent<FamilyExpression>,
    },
    PolyFamilyFn {
        family: Lexeme,
        identifier: Lexeme,
        lbracket: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rbracket: Lexeme,
        lparen: Lexeme,
        value_parameters: Constituent<ValueParameters>,
        rparen: Lexeme,
        coloneq: Lexeme,
        family_expression: Constituent<FamilyExpression>,
    },
    PolyFamilyFnSubtype {
        family: Lexeme,
        identifier: Lexeme,
        lbracket: Lexeme,
        type_parameters: Constituent<TypeParameters>,
        rbracket: Lexeme,
        lparen: Lexeme,
        value_parameters: Constituent<ValueParameters>,
        rparen: Lexeme,
        subtype: Lexeme,
        type_expression: Constituent<TypeExpression>,
        coloneq: Lexeme,
        family_expression: Constituent<FamilyExpression>,
    },
}

pub enum Definition {
    TypeDefinition {
        type_definition: Constituent<TypeDefinition>,
    },
    ValueDefinition {
        value_definition: Constituent<ValueDefinition>,
    },
    FamilyDefinition {
        family_definition: Constituent<FamilyDefinition>,
    },
    TypeDefinitionWhere {
        type_definition: Constituent<TypeDefinition>,
        where_: Lexeme,
        definition: Constituent<Definition>,
    },
    ValueDefinitionWhere {
        value_definition: Constituent<ValueDefinition>,
        where_: Lexeme,
        definition: Constituent<Definition>,
    },
    FamilyDefinitionWhere {
        family_definition: Constituent<FamilyDefinition>,
        where_: Lexeme,
        definition: Constituent<Definition>,
    },
}

pub enum Definitions {
    One {
        definition: Constituent<Definition>,
    },
    More {
        definition: Constituent<Definition>,
        definitions: Constituent<Definitions>,
    },
}

pub enum Program {
    Definitions {
        definitions: Constituent<Definitions>,
    },
}
