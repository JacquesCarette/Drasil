function tests = invalidInputTest
tests = functiontests(localfunctions);
end

function setupOnce(testCase)
%addpath('Testing');
addpath('FaultyInput');
addpath('/nfs/u50/grabm/PCM/Modules');
end

function testFI1(testCase)
f = @()main('FI01.txt');
verifyError(testCase, f, 'input:L');
end

function testFI2(testCase)
f = @()main('FI02.txt');
verifyError(testCase, f, 'input:L');
end

function testFI3(testCase)
f = @()main('FI03.txt');
verifyError(testCase, f, 'input:diam');
end

function testFI4(testCase)
f = @()main('FI04.txt');
verifyError(testCase, f, 'input:diam');
end

function testFI5(testCase)
f = @()main('FI05.txt');
verifyError(testCase, f, 'input:Vp');
end

function testFI6(testCase)
f = @()main('FI06.txt');
verifyError(testCase, f, 'input:Vp');
end

function testFI7(testCase)
f = @()main('FI07.txt');
verifyError(testCase, f, 'input:VpVt');
end

function testFI8(testCase)
f = @()main('FI08.txt');
verifyError(testCase, f, 'input:VpVt');
end

function testFI9(testCase)
f = @()main('FI09.txt');
verifyError(testCase, f, 'input:Ap');
end

function testFI10(testCase)
f = @()main('FI10.txt');
verifyError(testCase, f, 'input:Ap');
end

function testFI11(testCase)
f = @()main('FI11.txt');
verifyError(testCase, f, 'input:rho_p');
end

function testFI12(testCase)
f = @()main('FI12.txt');
verifyError(testCase, f, 'input:rho_p');
end

function testFI13(testCase)
f = @()main('FI13.txt');
verifyError(testCase, f, 'input:Tmelt');
end

function testFI14(testCase)
f = @()main('FI14.txt');
verifyError(testCase, f, 'input:Tmelt');
end

function testFI15(testCase)
f = @()main('FI15.txt');
verifyError(testCase, f, 'input:Tmelt');
end

function testFI16(testCase)
f = @()main('FI16.txt');
verifyError(testCase, f, 'input:C_ps');
end

function testFI17(testCase)
f = @()main('FI17.txt');
verifyError(testCase, f, 'input:C_ps');
end

function testFI18(testCase)
f = @()main('FI18.txt');
verifyError(testCase, f, 'input:C_pl');
end

function testFI19(testCase)
f = @()main('FI19.txt');
verifyError(testCase, f, 'input:C_pl');
end

function testFI20(testCase)
f = @()main('FI20.txt');
verifyError(testCase, f, 'input:Hf');
end

function testFI21(testCase)
f = @()main('FI21.txt');
verifyError(testCase, f, 'input:Hf');
end

function testFI22(testCase)
f = @()main('FI22.txt');
verifyError(testCase, f, 'input:Ac');
end

function testFI23(testCase)
f = @()main('FI23.txt');
verifyError(testCase, f, 'input:Ac');
end

function testFI24(testCase)
f = @()main('FI24.txt');
verifyError(testCase, f, 'input:Tmelt');
end

function testFI25(testCase)
f = @()main('FI25.txt');
verifyError(testCase, f, 'input:Tmelt');
end

function testFI26(testCase)
f = @()main('FI26.txt');
verifyError(testCase, f, 'input:Tc');
end

function testFI27(testCase)
f = @()main('FI27.txt');
verifyError(testCase, f, 'input:Tc');
end

function testFI28(testCase)
f = @()main('FI28.txt');
verifyError(testCase, f, 'input:rho_w');
end

function testFI29(testCase)
f = @()main('FI29.txt');
verifyError(testCase, f, 'input:rho_w');
end

function testFI30(testCase)
f = @()main('FI30.txt');
verifyError(testCase, f, 'input:C_w');
end

function testFI31(testCase)
f = @()main('FI31.txt');
verifyError(testCase, f, 'input:C_w');
end

function testFI32(testCase)
f = @()main('FI32.txt');
verifyError(testCase, f, 'input:hc');
end

function testFI33(testCase)
f = @()main('FI33.txt');
verifyError(testCase, f, 'input:hc');
end

function testFI34(testCase)
f = @()main('FI34.txt');
verifyError(testCase, f, 'input:hp');
end

function testFI35(testCase)
f = @()main('FI35.txt');
verifyError(testCase, f, 'input:hp');
end

function testFI36(testCase)
f = @()main('FI36.txt');
verifyError(testCase, f, 'input:Tinit');
end

function testFI37(testCase)
f = @()main('FI37.txt');
verifyError(testCase, f, 'input:Tinit');
end

function testFI38(testCase)
f = @()main('FI38.txt');
verifyError(testCase, f, 'input:TcTinit');
end

function testFI39(testCase)
f = @()main('FI39.txt');
verifyError(testCase, f, 'input:TcTinit');
end

function testFI40(testCase)
f = @()main('FI40.txt');
verifyError(testCase, f, 'input:TinitTmelt');
end

function testFI41(testCase)
f = @()main('FI41.txt');
verifyError(testCase, f, 'input:TcTinit');
end

function testFI42(testCase)
f = @()main('FI42.txt');
verifyError(testCase, f, 'input:TcTinit');
end

function testFI43(testCase)
f = @()main('FI43.txt');
verifyError(testCase, f, 'input:tfinal');
end

function testFI44(testCase)
f = @()main('FI44.txt');
verifyError(testCase, f, 'input:tfinal');
end

