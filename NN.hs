type Neuron = ([Double],Double) -- [weights], balance
type Layer = [Neuron]
type Net = [Layer]

-- define the specific nets that the site has, so we can test

nand :: Neuron
nand = ([-2,-2],3)

testnand = ((perceive $ zvar [0,0] nand) == 1) && ((perceive $ zvar [1,1] nand) == 0)

-- this is the z-variable, i.e. z = sum(w_j*x_j)+b
zvar :: [Double] -> Neuron -> Double
zvar inputs (weights,bias) = (foldl (+) 0 (zipWith (*) weights inputs)) + bias

-- what a perceptron does
perceive :: Double -> Double
perceive z = case (signum z) of
    -1 -> 0
    0  -> 0
    1  -> 1

-- σ(z)
sigma :: Double -> Double
sigma z = 1 / (1 + (exp (-z)))

-- for each neuron in the layer, the output is <function> . zvar
compute :: Net -> [Double] -> [Double]
compute (layer:layers) inputs = compute layers outputs
    where outputs = (map (perceive . (zvar inputs)) layer)
