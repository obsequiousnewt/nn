type Neuron = ([Double],Double) -- [weights], balance
type Layer = [Neuron]
type Net = [Layer]

-- define the specific nets that the site has, so we can test

nand :: Neuron
nand = ([-2,-2],3)

testnand = ((perceive $ zvar [0,0] nand) == 1) && ((perceive $ zvar [1,1] nand) == 0)

-- vector multiply, which we use a couple times.
vecmul :: [Double] -> [Double] -> Double
vecmul x y = sum (zipWith (*) x y)

-- this is the z-variable, i.e. z = sum(w_j*x_j)+b
zvar :: [Double] -> Neuron -> Double
zvar inputs (weights,bias) = (vecmul weights inputs) + bias

-- what a perceptron does
perceive :: Double -> Double
perceive z = case (signum z) of
    -1 -> 0
    0  -> 0
    1  -> 1

-- σ(z)
sigma :: Double -> Double
sigma z = 1 / (1 + (exp (-z)))

-- σ'(σ(z)) <PASS THIS σ(z) NOT z>
sigma' :: Double -> Double
sigma' s = s * (1-s)

-- for each neuron in the layer, the output is <function> . zvar
compute :: [Double] -> Net -> [Double]
compute inputs [] = inputs -- outputs really
compute inputs (layer:layers) = compute outputs layers
    where outputs = map (sigma . (zvar inputs)) layer


-- now for the backpropogation algorithm.
-- every layer needs to know both its own z values, and the errors (δ) of the layer above it.
-- we want to return a matrix of errors (errors are associated with neurons), which the <whatever calls this> can use to calculate the change in weight. (the change in weight is - (η * δ * a), and η is const and we already know a, so δ is all we need.)

-- meanwhile, δ = [w`*δ`]*σ'(z).

--           inputs             goals       errors
backprop :: [Double] -> Net -> [Double] -> [[Double]]
backprop inputs (layer:layers)  goals = (map (\neuron -> (vecmul (fst neuron) (head errors)) * (sigma' $ zvar inputs neuron)) layer) : errors
  where errors  = backprop outputs layers goals
        outputs = map (sigma . (zvar inputs)) layer

-- and the final case. here we have to compute the cost, then we multiply it by σ'(z).
backprop inputs [] goals = [zipWith (*) nablacost (map sigma' inputs)]
  where nablacost = zipWith (cost_quad') inputs goals

-- I'm not sure if we need this (yet?)
cost_quad :: [Double] -> [Double] -> Double
cost_quad inputs goals = 0.5 * (sum (zipWith (\x y -> (x-y)^2) inputs goals))

-- partial derivative, with respect to the first argument (inputs)
cost_quad' :: Double-> Double -> Double
cost_quad' input goal = input - goal
