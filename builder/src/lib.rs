use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DeriveInput, Error, Fields,
    GenericArgument, Lit, Meta, MetaList, MetaNameValue, NestedMeta, Path, PathArguments,
    PathSegment, Result, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive_builder(input) {
        Ok(token) => TokenStream::from(token),
        Err(err) => TokenStream::from(err.to_compile_error()),
    }
}

fn derive_builder(input: DeriveInput) -> Result<TokenStream2> {
    let name = format_ident!("{}", input.ident);
    let builder_name = format_ident!("{}Builder", input.ident);

    let struct_data = match input.data {
        Data::Struct(data) => data,
        _ => {
            return Err(Error::new_spanned(
                input,
                "Builder currently supports only struct",
            ));
        }
    };

    let (fields, initials, setters, validations, assignments) = match struct_data.fields {
        Fields::Named(fields) => {
            let fs = fields.named.iter().map(|v| {
                let ident = &v.ident;
                let typ = &v.ty;

                if type_is_option(typ) {
                    quote! { #ident: #typ }
                } else {
                    quote! { #ident: ::std::option::Option<#typ> }
                }
            });
            let initials = fields.named.iter().map(|v| {
                let ident = &v.ident;

                quote! { #ident: ::std::option::Option::None }
            });

            let mut setters = vec![];

            for v in fields.named.iter() {
                let ident = &v.ident;
                let typ = if type_is_option(&v.ty) {
                    unwrap_type_of_option(&v.ty).unwrap()
                } else {
                    v.ty.clone()
                };

                let mut attrs = vec![];

                for attr in v.attrs.iter() {
                    match attr.parse_meta() {
                        Ok(meta) => match meta {
                            Meta::List(MetaList {
                                ref path,
                                paren_token: _,
                                ref nested,
                            }) => {
                                if path.get_ident().map(|i| i == "builder").is_none() {
                                    continue;
                                }

                                if let Some(nm) = nested.first() {
                                    match nm {
                                        NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                                            ref path,
                                            eq_token: _,
                                            lit: Lit::Str(ref litstr),
                                        })) => {
                                            if path
                                                .get_ident()
                                                .map(|i| i != "each")
                                                .unwrap_or(false)
                                            {
                                                return Err(Error::new_spanned(
                                                    meta,
                                                    r#"expected `builder(each = "...")`"#,
                                                ));
                                            }

                                            if !type_is_vec(&v.ty) {
                                                return Err(Error::new_spanned(
                                                    nested,
                                                    "each is only available for Vec field",
                                                ));
                                            }

                                            attrs.push(Attr::Each(format_ident!(
                                                "{}",
                                                litstr.value()
                                            )));
                                        }
                                        _ => continue,
                                    }
                                }
                            }
                            _ => continue,
                        },
                        _ => continue,
                    }
                }

                let mut overwritten = false;
                attrs.into_iter().for_each(|v| {
                    let Attr::Each(fn_name) = v;
                    let typ = unwrap_type_of_vec(&typ).unwrap();
                    let ident = ident.as_ref().unwrap();

                    overwritten = ident == &fn_name;

                    setters.push(quote! {
                        fn #fn_name(&mut self, v: #typ) -> &mut Self {
                            match self.#ident {
                                ::std::option::Option::Some(ref mut values) => values.push(v),
                                _ => {
                                    self.#ident = ::std::option::Option::Some(vec![v]);
                                }
                            }

                            self
                        }
                    });
                });

                if !overwritten {
                    setters.push(quote! {
                        fn #ident(&mut self, v: #typ) -> &mut Self {
                            self.#ident = ::std::option::Option::Some(v);
                            self
                        }
                    });
                }
            }
            let validations = fields.named.iter().map(|v| {
                let ident = &v.ident;
                let typ = &v.ty;

                if type_is_option(typ) || type_is_vec(typ) {
                    quote! {}
                } else {
                    let msg = format!("{} is not set", ident.as_ref().unwrap().to_string());

                    quote! {
                        if self.#ident.is_none() {
                            return ::std::result::Result::Err(#msg.into());
                        }
                    }
                }
            });
            let assignments = fields.named.iter().map(|v| {
                let ident = &v.ident;
                let typ = &v.ty;

                if type_is_option(typ) {
                    quote! {
                        #ident: self.#ident.clone(),
                    }
                } else if type_is_vec(typ) {
                    quote! {
                        #ident: self.#ident.clone().unwrap_or(::std::vec::Vec::new()),
                    }
                } else {
                    quote! {
                        #ident: self.#ident.clone().unwrap(),
                    }
                }
            });

            (
                quote! {
                    #(#fs),*
                },
                quote! {
                    #(#initials),*
                },
                quote! {
                    #(#setters)*
                },
                quote! {
                    #(#validations)*
                },
                quote! {
                    #(#assignments)*
                },
            )
        }
        _ => {
            return Err(Error::new_spanned(
                struct_data.fields,
                "Builder currently supports only named fields",
            ));
        }
    };

    let token = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #initials
                }
            }

        }

        pub struct #builder_name {
            #fields,
        }

        impl #builder_name {
            pub fn build(&mut self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn std::error::Error>> {
                #validations

                ::std::result::Result::Ok(#name {
                    #assignments
                })
            }

            #setters
        }
    };

    Ok(token)
}

enum Attr {
    Each(Ident),
}

fn type_is_option(typ: &Type) -> bool {
    is_type(typ, "Option")
}

fn type_is_vec(typ: &Type) -> bool {
    is_type(typ, "Vec")
}

fn is_type(typ: &Type, name: &str) -> bool {
    match typ {
        &Type::Path(TypePath {
            path: Path { ref segments, .. },
            ..
        }) => match segments.last() {
            Some(seg) => seg.ident == name,
            _ => false,
        },
        _ => false,
    }
}

fn unwrap_type_of_option(typ: &Type) -> Option<Type> {
    unwrap_type(typ, "Option")
}

fn unwrap_type_of_vec(typ: &Type) -> Option<Type> {
    unwrap_type(typ, "Vec")
}

fn unwrap_type(typ: &Type, name: &str) -> Option<Type> {
    match typ {
        &Type::Path(TypePath {
            path: Path { ref segments, .. },
            ..
        }) => match segments.last() {
            Some(PathSegment {
                ident,
                arguments:
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
            }) => {
                if ident != name {
                    return None;
                }

                for arg in args.iter() {
                    match arg {
                        GenericArgument::Type(t) => return Some(t.clone()),
                        _ => continue,
                    }
                }

                return None;
            }
            _ => None,
        },
        _ => None,
    }
}
