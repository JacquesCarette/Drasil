module Modules.ReadTable (readTable) where

import Language.Drasil.Code
import Defs
import Prelude hiding (return)

readTable :: Module
readTable = buildModule "ReadTable" [] [] [read_z_array_func, read_x_array_func, read_y_array_func] []

read_z_array_func :: FunctionDecl
read_z_array_func = pubMethod methodTypeVoid "read_z_array" [p_filename] 
    [ 
      block [
        varDec l_infile infile,
        openFileR v_infile v_filename,
        varDec l_line string,
        getFileInput v_infile string v_line,
        closeFile v_infile,
        listDec' l_z_array_str string 0,
        stringSplit v_z_array_str v_line ",",
        listSlice v_z_array_str v_z_array_str (Just $ litInt 1) Nothing (Just $ litInt 2),
        listDec' l_z_array float 0,
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_z_array_str$.listSize) ((&++) v_i) 
          (oneLiner $ valStmt $ v_z_array$.(listAppend $ (v_z_array_str$.(listAccess v_i)$.(cast float)))),
        return v_z_array
      ]
    ]     

read_x_array_func :: FunctionDecl
read_x_array_func = pubMethod methodTypeVoid "read_x_array" [p_filename] 
    [ 
      block [
        varDec l_infile infile,
        openFileR v_infile v_filename,
        listDec' l_lines string 0,
        getFileInputAll v_infile v_lines,
        closeFile v_infile,
        listSlice v_lines v_lines (Just $ litInt 1) Nothing Nothing,
        listDec' l_x_array_str (listT string) 0,
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_lines$.listSize) ((&++) v_i)
          [
            block [
              listDec' "temp_str" string 0,
              stringSplit (var "temp_str") (v_lines$.(listAccess v_i)) ",",
              listSlice (var "temp_str") (var "temp_str") (Just $ litInt 0) Nothing (Just $ litInt 2),
              valStmt $ v_x_array_str$.(listAppend (var "temp_str"))
            ]
          ],
        listDec' l_x_array (listT float) 0,
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_x_array_str$.listSize) ((&++) v_i)
          [
            block [
              listDec' l_nextLine float 0,
              for (varDecDef l_j int (litInt 0)) (v_j ?< v_x_array_str$.(listAccess v_i)$.listSize) ((&++) v_j)
                (oneLiner $ valStmt$ v_nextLine$.(listAppend $ v_x_array_str$.(listAccess v_i)$.(listAccess v_j)$.(cast float))),
              valStmt $ v_x_array$.(listAppend v_nextLine)                
            ]
          ],
        return v_x_array
      ]
    ]     

read_y_array_func :: FunctionDecl
read_y_array_func = pubMethod methodTypeVoid "read_y_array" [p_filename] 
    [ 
      block [
        varDec l_infile infile,
        openFileR v_infile v_filename,
        listDec' l_lines string 0,
        getFileInputAll v_infile v_lines,
        closeFile v_infile,
        listSlice v_lines v_lines (Just $ litInt 1) Nothing Nothing,
        listDec' l_y_array_str (listT string) 0,
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_lines$.listSize) ((&++) v_i)
          [
            block [
              listDec' "temp_str" string 0,
              stringSplit (var "temp_str") (v_lines$.(listAccess v_i)) ",",
              listSlice (var "temp_str") (var "temp_str") (Just $ litInt 1) Nothing (Just $ litInt 2),
              valStmt $ v_y_array_str$.(listAppend (var "temp_str"))
            ]
          ],
        listDec' l_y_array (listT float) 0,
        for (varDecDef l_i int (litInt 0)) (v_i ?< v_y_array_str$.listSize) ((&++) v_i)
          [
            block [
              listDec' l_nextLine float 0,
              for (varDecDef l_j int (litInt 0)) (v_j ?< v_y_array_str$.(listAccess v_i)$.listSize) ((&++) v_j)
                (oneLiner $ valStmt$ v_nextLine$.(listAppend $ v_y_array_str$.(listAccess v_i)$.(listAccess v_j)$.(cast float))),
              valStmt $ v_y_array$.(listAppend v_nextLine)                
            ]
          ],
        return v_y_array
      ]
    ]     