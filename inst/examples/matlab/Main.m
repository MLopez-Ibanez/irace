% function Result = Main(INSTANCE,SEED,A, B)
% Using INSTANCE as a seed, this function creates a pseudo-random number 
% drawn from a uniform distribution in the open interval (-1,1) and returns
% the value of Result, where Result = A*B + r
function Result = Main(INSTANCE,SEED,A,B)

rng('default');
rng(SEED);

minValue = -1;
maxValue = 1;
r = (maxValue - minValue) * rand(1) + minValue;
Result = A * B + r;
% fprintf('Result for irace=%g\n', Result);
fid=fopen('Result.txt','wt');
fprintf(fid,'%g\n',Result);
fclose(fid);

end
