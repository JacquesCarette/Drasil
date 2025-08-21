#!/usr/bin/env python3
import re

def fix_either_patterns(content):
    # Fix patterns for binary operations - case matching
    content = re.sub(r'\(Left (\w+), Left (\w+)\)', r'(Right \1, Right \2)', content)
    content = re.sub(r'\(Left (\w+), Right (\w+)\)', r'(Right \1, Left \2)', content)
    content = re.sub(r'\(Right (\w+), Left (\w+)\)', r'(Left \1, Right \2)', content)
    content = re.sub(r'\(Right (\w+), Right (\w+)\)', r'(Left \1, Left \2)', content)
    
    # Fix patterns for unary operations - case matching
    content = re.sub(r'case infer cxt (\w+) of\s+Left (\w+) ->', r'case infer cxt \1 of\n    Right \2 ->', content)
    
    # Fix return patterns - success should be Right
    content = re.sub(r'then Left ([A-Z]\w*(?:\.[A-Z]\w*)*)', r'then Right \1', content)
    content = re.sub(r'-> Left ([A-Z]\w*(?:\.[A-Z]\w*)*)', r'-> Right \1', content)
    
    # Fix return patterns - errors should be Left  
    content = re.sub(r'else Right \$', r'else Left $', content)
    content = re.sub(r'-> Right \$', r'-> Left $', content)
    
    # Fix error propagation patterns
    content = re.sub(r'\(_, Right (\w+)\) -> Right \1', r'(_, Left \1) -> Left \1', content)
    content = re.sub(r'\(Right (\w+), _\) -> Right \1', r'(Left \1, _) -> Left \1', content)
    content = re.sub(r'\(_\s*,\s*Right (\w+)\) -> Right \1', r'(_, Left \1) -> Left \1', content)
    content = re.sub(r'\(Right (\w+),\s*_\) -> Right \1', r'(Left \1, _) -> Left \1', content)
    
    return content

if __name__ == "__main__":
    with open('drasil-lang/lib/Language/Drasil/Expr/Lang.hs', 'r') as f:
        content = f.read()
    
    fixed_content = fix_either_patterns(content)
    
    with open('drasil-lang/lib/Language/Drasil/Expr/Lang.hs', 'w') as f:
        f.write(fixed_content)
    
    print("Applied comprehensive Either pattern fixes")
