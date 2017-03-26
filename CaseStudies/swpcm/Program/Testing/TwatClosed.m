function Twat = TwatClosed(filename)

[ params ] = load_params(filename);

i = 1;

for t = [3322.065744:2:20571.368997];
  Twat(i, 1) = t;
  Twat(i, 2) = ((params.Tinit - params.Tc)*exp(1)^(-1*(params.eta+1)*t)+params.Tinit*params.eta+params.Tc)/(params.eta+1);
  i = i+1;
  end