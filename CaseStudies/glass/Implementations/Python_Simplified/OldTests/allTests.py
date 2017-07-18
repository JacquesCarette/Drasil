from .testCalculations import calcTests
# from .testCheckConstraints import checkConTests
# from .testDerivedValues import derivedValTests
# from .testInputFormat import inputFormatTests
# from .testInterp import interpTests
# from .testMainfun import mainTests
# from .testOutputFormat import outputFormatTests
# from .testReadTable import readTabTests

all_results = sum ([calcTests.CalcTests.calc_results])
                    # checkConTests.CheckConTests.checkCon_results,
                    # derivedValTests.DerivedValTests.derivedVal_results,
                    # inputFormatTests.InputFormatTests.inputFormat_results,
                    # interpTests.InterpTests.interp_results,
                    # mainTests.MainTests.main_results,
                    # outputFormatTests.OutputFormatTests.outputFormat_results,
                    # readTabTests.ReadTabTests.readTab_results])
                        
all_failures = sum ([calcTests.CalcTests.calc_failures])
                     # checkConTests.CheckConTests.checkCon_failures,
                     # derivedValTests.DerivedValTests.derivedVal_failures,
                     # inputFormatTests.InputFormatTests.inputFormat_failures,
                     # interpTests.InterpTests.interp_failures,
                     # mainTests.MainTests.main_failures,
                     # outputFormatTests.OutputFormatTests.outputFormat_failures,
                     # readTabTests.ReadTabTests.readTab_failures])
                     
all_successes = all_results - all_failures

print("Ran " + str(all_results) + " tests in total. " + str(all_successes) + " passed and " +\
str(all_failures) + " failed.")
