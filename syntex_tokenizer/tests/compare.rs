extern crate syntex_tokenizer;
extern crate rustlexspec;


#[test]
fn keywords() { compare_on("\
abstract	alignof	as	become	box
break	const	continue	crate	do
else	enum	extern	false	final
fn	for	if	impl	in
let	loop	macro	match	mod
move	mut	offsetof	override	priv
proc	pub	pure	ref	return
Self	self	sizeof	static	struct
super	trait	true	type	typeof
unsafe	unsized	use	virtual	where
while	yield
")
}

#[test]
fn keywords_gready() { compare_on("\
abstractx	alignofx	axs	becomex	boxx
break92	const92	continue92	crate92	do92
else92	enum92	extern92	false92	final92
fn92	for92	if92	impl92	in92
let92	loop92	macro92	match92	mod92
move__	mut__	offsetof__	override__	priv__
proc__	pub__	pure__	ref__	return__
Self__	self__	sizeof__	static__	struct__
super__	trait__	true__	type__	typeof__
unsafe__	unsized__	use__	virtual__	where__
while__	yield__
")
}

fn compare_on(text: &str) {
    let expected = rustlexspec::tokenize(text).unwrap();
    let actual = syntex_tokenizer::tokenize(text).expect(&format!(
        "Failed to parse `{}`", text
    ));
    let mut o = 0;
    for (&e, &a) in expected.iter().zip(actual.iter()) {
        let ename = e.token_type.name();
        let aname = e.token_type.name();
        if ename != aname || e.len != a.len {
            panic!("\nExpected: {} {:?}\n\
                      Actual  : {} {:?}\n",
                    ename, &text[o..o+e.len],
                    aname, &text[o..o+a.len]);
        }
        o += e.len;
    }
    assert_eq!(actual.len(), expected.len());
}