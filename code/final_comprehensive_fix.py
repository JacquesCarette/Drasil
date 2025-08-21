#!/usr/bin/env python3
import re

def fix_all_either_patterns(content):
    """Comprehensive fix for all Either patterns in the Haskell file"""
    
    # 1. Fix ALL binary operation patterns - most important
    # Success cases: (Left, Left) -> (Right, Right)
    content = re.sub(r'\(Left ([a-zA-Z][a-zA-Z0-9@_\.]*), Left ([a-zA-Z][a-zA-Z0-9@_\.]*)\)', r'(Right \1, Right \2)', content)
    
    # Error propagation cases: (_, Right) -> (_, Left) and (Right, _) -> (Left, _)
    content = re.sub(r'\(_, Right ([a-zA-Z][a-zA-Z0-9]*)\) -> Right \1', r'(_, Left \1) -> Left \1', content)
    content = re.sub(r'\(Right ([a-zA-Z][a-zA-Z0-9]*), _\) -> Right \1', r'(Left \1, _) -> Left \1', content)
    content = re.sub(r'\(_\s*,\s*Right ([a-zA-Z][a-zA-Z0-9]*)\) -> Right \1', r'(_, Left \1) -> Left \1', content)
    content = re.sub(r'\(Right ([a-zA-Z][a-zA-Z0-9]*),\s*_\) -> Right \1', r'(Left \1, _) -> Left \1', content)
    
    # 2. Fix unary operation patterns
    # Case matching: Left -> Right for successful inferences
    content = re.sub(r'case infer cxt ([a-zA-Z][a-zA-Z0-9]*) of\s*Left ([a-zA-Z][a-zA-Z0-9@_\.]*) ->', 
                     r'case infer cxt \1 of\n    Right \2 ->', content)
    
    # General Left pattern matching in case expressions
    content = re.sub(r'Left ([a-zA-Z][a-zA-Z0-9@_\.]*) ->', r'Right \1 ->', content)
    
    # 3. Fix return value patterns
    # Success returns: Left Space -> Right Space
    content = re.sub(r'then Left ([A-Z][a-zA-Z0-9\.]*)', r'then Right \1', content)
    content = re.sub(r'-> Left ([A-Z][a-zA-Z0-9\.]*)', r'-> Right \1', content)
    
    # Error returns: Right $ -> Left $
    content = re.sub(r'else Right \$', r'else Left $', content)
    content = re.sub(r'-> Right \$', r'-> Left $', content)
    
    # 4. Fix specific patterns that might be missed
    # Pattern: Left something@ -> Right something@
    content = re.sub(r'Left ([a-zA-Z][a-zA-Z0-9]*@[^)]*) ->', r'Right \1 ->', content)
    
    # Mixed error patterns in more complex cases
    content = re.sub(r'\(Left ([a-zA-Z][a-zA-Z0-9@_\.]*), Right ([a-zA-Z][a-zA-Z0-9]*)\)', r'(Right \1, Left \2)', content)
    content = re.sub(r'\(Right ([a-zA-Z][a-zA-Z0-9]*), Left ([a-zA-Z][a-zA-Z0-9@_\.]*)\)', r'(Left \1, Right \2)', content)
    
    return content

def main():
    file_path = 'drasil-lang/lib/Language/Drasil/Expr/Lang.hs'
    
    # Read the file
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Apply comprehensive fixes
    fixed_content = fix_all_either_patterns(content)
    
    # Write back the fixed content
    with open(file_path, 'w') as f:
        f.write(fixed_content)
    
    print("Applied comprehensive Either pattern fixes to all functions")

if __name__ == "__main__":
    main()
