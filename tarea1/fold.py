# def debug_fold_prop(func_name, args, var_f, and_f, or_f, not_f):
#     return f"""(define {func_name} (prop {args}))
#         (match prop
#             [(varp v) ({var_f} {args})]
#             [(andp p1 p2) ({and_f} ({func_name} p1) ({func_name} p2))]
#             [(orp p1 p2) ({or_f} ({func_name} p1) ({func_name} p2))]
#             [(notp p) ({not_f} ({func_name} p))]"""


# def parse_args(args: list[str]):
#     return f"({', '.join(args)})"


def debug_fold_prop(
    func_name: str, args: list[str], var_f: str, and_f: str, or_f: str, not_f: str
):
    return """(define {func_name} (prop {args}))
    (match prop
        [(varp v) ({var_f}) v]
        [(andp p1 p2) ({and_f} ({func_name} p1 {args}) ({func_name} p2 {args}))]
        [(orp p1 p2) ({or_f} ({func_name} p1 {args}) ({func_name} p2 {args}))]
        [(notp p) ({not_f} ({func_name} p {args}))]""".format(
        func_name=func_name,
        args=' '.join(args),
        var_f=var_f,
        and_f=and_f,
        or_f=or_f,
        not_f=not_f,
    )
    


if __name__ == "__main__":
    print(
        debug_fold_prop(
            "occurrences",
            ["var"],
            "(lambda (x) (if (string=? x var) 1 0))",
            "(lambda (x y) (+ x y))",
            "(lambda (x y) (+ x y))",
            "(lambda (x) x)",   
        )
    )
    
    print(
        debug_fold_prop(
            "simplify-negations",
            [],
            "(lambda (r) (varp r))",
            "(lambda (p q) (andp p q))",
            "(lambda (p q) (orp p q))",
            
    )
    