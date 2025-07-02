import os
import subprocess
from termcolor import colored

print('\n ------ Testing ------ \n')

total, correct = 0, 0
for in_f in os.listdir('input'):
    if '-' not in in_f: continue

    out_f = in_f.replace('input', 'output')
    in_f = f'input/{in_f}'
    exp_f = in_f.replace('input', 'expected')
    print(f'File: {in_f}')

    # execute the file
    subprocess.run(['runghc', 'ws.hs', in_f])

    # if the code rann without errors then open the 
    # output and check against expected file
    if os.path.isfile(out_f):
        with open(out_f, 'r') as output, open(exp_f, 'r') as expected:
            expd = expected.readlines()
            recv = output.readlines()

            res = expd == recv
            correct += 1 if res else 0

            res_s = (
                colored('PASSED', 'green', None, ['bold']) if res else 
                colored('FAILED', 'red', None, ['bold'])
            )
            print(f'{expd}\n{recv}\n{res_s}\n')
        os.remove(out_f)
    else:
        print('Error encountered! Output file not created.')
    total +=1

result = colored(f'{correct}/{total}', 'cyan', None, ['bold'])
print(f'\n --- Passed {result} Tests ---- \n')