use err::CompileError;
use pest::Span;

type Mir<'src> = crate::stages::s1::MirS1<'src>;

pub(crate) fn check_main(mir: Mir) -> Result<Mir, CompileError> {
    let global = mir.scopes.get(&0).unwrap();
    if !global.funs.contains_key(&"main") {
        return Err(CompileError::new(
            Span::new("Missing 'main()' function", 0, 25).unwrap(),
            "".to_owned(),
        ));
    }

    let main = global.funs.get(&"main").unwrap();
    if !main.args.is_empty() {
        return Err(CompileError::new(
            main.span,
            "fn 'main()' can not have arguments".to_owned(),
        ));
    }

    Ok(mir)
}

#[test]
fn check_main_test<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"        
        fn main() -> i64 { return 0; }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = crate::stages::s1::compile_mir_s1(hir)?;
    println!("{mir:#?}");

    Ok(())
}
