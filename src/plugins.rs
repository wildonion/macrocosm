


use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use std::collections::{HashMap, HashSet as Set};
use std::f64::consts::E;
use std::io::Write;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, parse_quote, Data, DeriveInput, Expr, FnArg, Ident, ItemFn, Local, Pat, ReturnType, Stmt, Token, Type};

/*  > -------------------------------------------
    |           proc macro functions 
    | ------------------------------------------
    |
    |   RUST CODES ---> TOKEN STREAM ---> AST
    |
    | 0 - compiler generates TokenStreams of Rust codes that this proc macro is placed of top of
    | 1 - parse (a new parser perhaps!) TokenStreams (Rust codes) to generate AST using syn
    | 2 - write new Rust codes using the patterns inside the generated AST like mutating idents or variables
    | 3 - convert generated or mutated either pure Rust codes in step 2 into a new AST using quote
    | 4 - return the new AST as a new TokenStream to the compiler to update the method or struct field at compile time
    |

    _> the input to all methods is of type TokenStream which is the extracted tokens
       of the actual Rust codes that can be used to build the AST later by compiler

    https://veykril.github.io/tlborm/introduction.html
    https://blog.logrocket.com/procedural-macros-in-rust/
    https://danielkeep.github.io/tlborm/book/README.html
    https://developerlife.com/2022/03/30/rust-proc-macro/
    https://github.com/dtolnay/proc-macro-workshop

    since macro processing in Rust happens after the construction of the AST, as such, 
    the syntax used to invoke a macro must be a proper part of the language's syntax 
    tree thus by adding a new code in this crate the compiler needs to compile the whole 
    things again, which forces us to reload the workspace everytime, means by that any 
    logging codes don't work in here at runtime and we must check them in console once 
    the code gets compiled.

    a TokenStream is simply built from the Rust codes which can be used to built the
    AST like: RUST CODES ---> TOKEN STREAM ---> AST also the following are matters:
    sync generates : the AST from the passed in TokenStream (sequence of token trees)
    quote generates: Rust codes that can be used to generate TokenStream and a new AST

    proc macro can be on top of methods, union, enum and struct and can be used to add 
    method to them before they get compiled since compiler will extend the struct AST 
    by doing this once we get the token stream of the struct Rust code. it can be used 
    to parse it into Rust pattern (ident, ty, tt and ...) that will be used to add a new 
    or edit a logic on them finally we must use the extended token stream of the Rust codes 
    that we've added to convert them into a new token stream to return from the macro to 
    tell the compiler that extend the old AST with this new one

    kinds: 
        decl_macro
        proc_macro
        proc_macro_derive
        proc_macro_attribute
    
    benefits:
        add a method to struct or check a condition against its fields
        convert trait into module to extend the trait methods
        extend the interface of a struct by changing the behaviour of its fields and methods
        create a DSL like jsx, css or a new keyword or a new lang
        build a new AST from the input TokenStream by parsing incoming tokens and return the generated TokenStream from a new Rust codes
        write parser using decl_macro
        changing and analysing the AST logics of methods at compile time before getting into their body
        bind rust code to other langs and extending code in rust using macros
        extend the user code by adding some code into his already coded logic at compile time
    

*/

struct Args{
    vars: Set<Ident>
}

/*
    we need to create our own parser to parse the 
    args token stream into a new AST
*/
impl Parse for Args {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let vars = Punctuated::<Ident, Token![,]>::parse_terminated(input)?;
        Ok(Args {
            vars: vars.into_iter().collect(),
        })
    }
}

/*
    with the following proc macro we can do inspect and operate on the 
    api methods before generating the output or executing any extra
    logics before getting into the api body like actix #[get()] which
    checks the request path in the first place before sliding into 
    the request body, also to get the Rust token codes from TokenStream 
    we must use syn::parse and to get the TokenStream from Rust codes 
    we msut use quote

    ex:
    #[passport_proc(admin)]
    #[passport_proc(user)]
    #[passport_proc(dev)]
    #[passport_proc(access=admin,user,dev)]
    fn im_a_method(){}
*/
#[proc_macro_attribute]
pub fn passport(args: TokenStream, input: TokenStream) -> TokenStream {

    /*  
        build the new AST from the `input` TokenStream to extend the one that we 
        already have by using syn to parse the args & input tokens into a syntax 
        tree, note that the type of TokenStream that we want to parse it with syn 
        to generate AST, must be specified, like parsing a function TokenStream 
        into ItemFn AST, then we need to generate a new TokenStream from generated 
        Rust types parsed from the input TokenStream, using quote to do so, generate 
        a new TokenStream from the passed in Rust codes (either pure or using #variable) 
        to it that can be used to build a new AST, this will replace whatever `input` 
        is annotated with this attribute proc macro, finally we'll return the token 
        stream either generated by the quote or the passed in input.

        when we are defining a procedural macro, we're not actually interacting with 
        the runtime data, instead, we're generating code that will be inserted into 
        the function thus we can't access the token inside the request object in this 
        proc macro since procedural macros work at compile time, they don't have access 
        to runtime data, in our case, the token in the HTTP request header is available 
        at runtime, so it's impossible to directly inspect the header's content inside
        a procedural macro.
    */
    let mut api_ast = syn::parse::<syn::ItemFn>(input.clone()).unwrap(); /* parsing the input token stream or the method into the ItemFn AST */
    let roles_set = parse_macro_input!(args as Args).vars; /* casting the args TokenStream into the Args parser */
    let mut granted_roles = vec![];
    for role in roles_set{
        granted_roles.push(role.to_string()); /* converting the Ident into String */
    }

    /*  
        every variable can be shown as ident in Rust thus if we wanna have a new variable we must 
        create new ident instance, like the following for the request object, also every token 
        in a TokenStream has an associated Span holding some additional info, a span, is a region 
        of source code, along with macro expansion information, it points into a region of the 
        original source code(important for displaying diagnostics at the correct places) as well 
        as holding the kind of hygiene for this location. The hygiene is relevant mainly for 
        identifiers, as it allows or forbids the identifier from referencing things or being 
        referenced by things defined outside of the invocation.
    */
    let mut req_ident = syn::Ident::new("req", proc_macro2::Span::call_site());
    for input in api_ast.clone().sig.inputs{
        if let FnArg::Typed(pat_type) = input{
            if let Pat::Ident(pat_ident) = *pat_type.pat{
                if pat_ident.ident.to_string() == "req".to_string(){
                    req_ident = pat_ident.ident;
                    break;
                }
            }
        }
    }

    /* 
        generating a token stream from granted_roles variable, 
        quote generates new AST or token stream from Rust codes
        that can be returned to the proc macro caller since quote
        generates a token stream which is the return type of this
        proc macro methods
    */
    let new_stmt = syn::parse2(
        quote!{ /* building new token stream from the Rust token codes */
            
            /* 
                granted_roles can be accessible inside the api body at runtime, 
                vec![#(#granted_roles),*] means that we're pushing all the roles
                inside a vec![] and since there are multiple roles we used * to 
                push them all into the vec![] which means repetition pattern
            */
            let granted_roles = vec![#(#granted_roles),*]; // extending the AST of the api method at compile time

        }
    ).unwrap();

    /* injecting the granted_roles into the api body at compile time */
    api_ast.block.stmts.insert(0, new_stmt);
    
    /* 
        returning the newly generated AST by the quote of the input api Rust code  
        which contains the updated and compiled codes of the function body, at this
        stage we're building new token stream from the updated api_ast Rust token codes
    */
    TokenStream::from(quote!(#api_ast))


}
 
#[proc_macro_attribute]
pub fn go(args: TokenStream, input: TokenStream) -> TokenStream{

    // get the function
    // let async_task = parse_macro_input!(input as ItemFn);

    let async_task = syn::parse::<syn::ItemFn>(input.clone()).unwrap(); // get the function name
    
    // function signature: function name, args, visibility
    let async_task_name = &async_task.sig.ident;


    let output = quote!{
        // execute the async method using tokio main proc macro
        #[tokio::main]
        pub async fn #async_task_name() {
            #async_task
        }

        tokio::spawn(async move{
            #async_task
        });
    };

    output.into()

}

/* -------------------------------
    args are the inputs in proc macro like: #[gokio(arg1, arg2)] which arg1 and arg2 are 
    TokenStream presented to gokio proc macro, note that in every steps, after extending 
    the code we must convert the code into TokenStream using quote!{} and # to generate new 
    code and extend the function by injecting them to the body at compile time TokenStream 
    can be converted into actual code using # to be passed to quote!{} to generate new 
    TokenStream, procedural macros often use quote! to generate Rust code dynamically based 
    on the input TokenStream and it allows for dynamic code generation based on the input 
    tokens provided to the macro, so when we use # followed by a Rust token or expression
    within quote!, it will be replaced with the corresponding Rust code during code generation 
    at compile time, Example:
    
    let my_variable = 42;
    let generated_code = quote! {
        let x = #my_variable;
    };
*/
#[proc_macro_attribute]
pub fn gokio(args: TokenStream, input: TokenStream) -> TokenStream { 

    // -- syn parses TokenStream to AST and Rust codes like Stmt, ItemFn, Ident (name of fields, structs and functions), Data, Fields, ...
    // -- # allows us to interpolate generated and parsed Rust, AST and TokenStream codes into quote!{}  
    // -- quote!{} parses injected Rust, AST and TokenStream codes to generate TokenStream

    // extracting the function
    // let mut async_function = parse_macro_input!(input as ItemFn);
    let mut async_function = syn::parse::<syn::ItemFn>(input.clone()).unwrap(); // function is ItemFn with attrs, vis, sig and block

    // extracting function signature and return type, must be converted into TokenStream
    let function_ret_type = match &async_function.sig.output{
        ReturnType::Type(_, ty) => quote!{#ty},
        ReturnType::Default => quote!{ () }, // Default AST is ()
    };

    // extracting function args
    let arg_names = async_function.sig.inputs.iter().map(|arg|{
        match arg{
            // FnArg: an argument in a function signature: the n: usize in fn f(n: usize).
            syn::FnArg::Typed(pat_type) => { // getting the pattern and type of argument
                let ident = pat_type.pat.to_token_stream(); // convert the pattern type into token stream to interpolate into quote!{}
                quote!{#ident} // generate the TokenStream from the ident TokenStream which has built from the pat_type AST
            },
            _ => panic!("Unsupported function argument type"),
        }
    });

    // generate the code of mpsc channel
    let gokio_channel = quote!{
        let (tx, rx) = tokio::sync::mpsc::channel::<#function_ret_type>(1024);
    };

    // generate the code of spawning the async function into the tokio green 
    // threadpool, later then we send the result to the channel so user can 
    // get the data from the receiver inside the method body
    let spawn_function_code = quote!{
        tokio::spawn(async move{
            let result = #async_function(#(#arg_names),*).await; // executing the async task in the background
            tx.send(result).await.unwrap();
        });
    };

    let gokio_stmts = syn::parse2(
        quote!{
            #gokio_channel
            #spawn_function_code
        }   
    ).unwrap();


    async_function.block.stmts.insert(0, gokio_stmts);

    TokenStream::from(quote!(#async_function))

    // OR

    // combine the generated code with the original function body
    // let expanded = quote!{
    //     #async_function
    //     #gokio_channel // user can use rx to receive the data
    //     #spawn_function_code
    // };
    // // expanded.into()


}

#[proc_macro_attribute]
// we must extract Rust and real codes from AST corresponding the TokenStream
// then inject into existing code and convert the existing code into TokenStream
// ...
// defer statement is used to schedule a function call to be executed when the 
// surrounding function returns, they will be executed in reverse order of their appearance
pub fn defer(args: TokenStream, input: TokenStream) -> TokenStream { 

    // converting the input TokenStream into the ItemFn AST
    let mut func = syn::parse::<ItemFn>(input.clone()).unwrap();
    
    // AST contains token and Rust types that can be used to extract info about a type
    let fn_name = &func.sig.ident;

    // generate TokenStream from the AST and Rust codes
    let expanded = quote!{

        // note that defining structs and functions must be inside quote!{}
        // The defer procedural macro creates a new instance of Defer when invoked.
        // The Defer struct holds a closure that will be executed when an Defer instance is dropped.
        // The closure is executed in the Drop implementation of Defer, ensuring it runs when the surrounding scope exits which is about to be ended and died
        pub struct Defer<F: FnOnce()>{
            f: Option<F>
        }

        impl<F: FnOnce()> Defer<F>{
            fn new(f: F) -> Defer<F>{
                Defer::<F>{
                    f: Some(f)
                }
            }
        }

        // introduce generics and boundaries in impl<>
        impl<F: FnOnce()> Drop for Defer<F>{

            fn drop(&mut self) {
                if let Some(f) = self.f.take(){
                    f();
                }
            }
        } 

        let _ = Defer::new(||{
            // The error "cannot find value run in this scope" occurs because the run function is defined within 
            // the same module as the defer procedural macro, and the macro expansion is trying to access the run 
            // function in the same scope where it is not defined
            // The defer procedural macro now correctly references the function name using stringify!(#fn_name) 
            // to access the function name within the macro expansion.
            // The run function should be defined in the same module where the defer macro is used to ensure 
            // it is accessible during macro expansion.
            println!("deferred code executed for function: {}", stringify!(#fn_name));
        });

    };

    // generate TokenStream from the AST and Rust codes using quote!{}
    TokenStream::from(expanded)

}

// ✦•┈๑⋅⋯ ⋯⋅๑┈•✦
// જ⁀➴ ✅
// ✦•┈๑⋅⋯ ⋯⋅๑┈•✦
#[proc_macro_derive(SaveMe)]
// we must extract Rust and real codes from AST corresponding the TokenStream
// then inject into existing code and convert the existing code into TokenStream
pub fn saveme(input: TokenStream) -> TokenStream { 

    // -- syn parses TokenStream to AST and Rust codes like Stmt, ItemFn, Ident (name of fields, structs and functions), Data, Fields ...
    // -- # allows us to interpolate generated and parsed Rust, AST and TokenStream codes into quote!{}  
    // -- quote!{} parses injected Rust, AST and TokenStream codes to generate TokenStream
    
    // converting TokenStream into DeriveInput AST since the type which 
    // has annotated with this macro is struct we go for parsing the struct
    let derive_input = syn::parse::<DeriveInput>(input.clone()).unwrap();

    // extracting structure name and fields
    let struct_name = &derive_input.ident;
    let fields = match derive_input.data{
        Data::Struct(data) => data.fields,
        _ => panic!("expected struct"),
    };

    // save struct into a file
    let str_struct_path = format!("{}.json", struct_name.to_string());
    let mut file = std::fs::File::create(&str_struct_path).unwrap(); // can't use ? since the return type is not of type Result<>
    let mut field_infos = vec![];
    for f in fields{
        field_infos.push(
            f.ident.unwrap().to_string()
        )
    }
    let struct_data = serde_json::json!({
        "name": struct_name.to_string(),
        "fields": field_infos
    });
    let stringified_value = serde_json::to_string_pretty(&struct_data).unwrap();
    file.write_all(stringified_value.as_bytes()).unwrap();

    // code injection goes into quote!{}
    let output = quote! {
        
    };

    // Return the modified token stream
    output.into()

    // don't return the input itself cause compiler says:
    // name `Data` is defined multiple times `Data` must be defined only once in the type namespace of this module
    // in which `Data` is the name of the struct that is being annotated with this macro

}  
