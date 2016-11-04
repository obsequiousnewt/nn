type Neuron = ([Double],Double) -- [weights], balance
type Layer = [Neuron]
type Net = [Layer]

-- test nets

nand :: Neuron
nand = ([-2,-2],3)

testnand = ((perceive $ zvar [0,0] nand) == 1) && ((perceive $ zvar [1,1] nand) == 0)

-- https://mattmazur.com/2015/03/17/a-step-by-step-backpropagation-example/

mazur :: Net
mazur = [[([0.15,0.20],0.35),([0.25,0.30],0.35)],
         [([0.40,0.45],0.60),([0.50,0.55],0.60)]]

-- tested—this gives at least as many digits of accuracy as the example on the site
testmazur = compute [0.05,0.10] mazur

-- I'm about 90% sure the values here are correct.
testmazurbp = backprop [0.05,0.10] mazur [0.01,0.99]

testmazurgd = descend [0.05,0.10] mazur [0.01,0.99]

----------------

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
-- every layer needs to know both its own z values, and the weights (w) and errors (δ) of the layer above it.
-- we can get weights easily; that's part of the net and we have access to everything below us.
-- we want to return a matrix of errors (errors are associated with neurons), which the <whatever calls this> can use to calculate the change in weight. (the change in weight is - (η * δ * a), and η is const and we already know a, so δ is all we need.)


--           inputs             goals       errors
backprop :: [Double] -> Net -> [Double] -> [[Double]]

-- the final case, where δ = cost' * σ'(z).
backprop inputs [layer] goals        = (zipWith (*) costs (map sigma' outputs)) : []
  where costs   = zipWith (cost_quad') outputs goals
        outputs = map (sigma . (zvar inputs)) layer

-- in all other cases, δ = [w`*δ`]*σ'(z).
backprop inputs (layer:layers) goals = (zipWith (*) (matmul (map fst $ head layers) (head errors)) (map sigma' outputs)) : errors
  where errors  = backprop outputs layers goals
        outputs = map (sigma . (zvar inputs)) layer

-- this isn't a generic matrix multiplication—rather, it's an implementation of the w`*δ` thing.
-- amusingly we could curry this, but you know what? fsck currying, it's a stupid way to obscure things.
-- we have to zipWith (map) to multiply the error of each neuron by each of its weights.
-- "then" we fold addition.
matmul :: [[Double]] -> [Double] -> [Double]
matmul weights errors = foldl (zipWith (+)) (repeat 0) $ zipWith (\w e -> map (*e) w) weights errors

-- I'm not sure if we need this (yet?)
cost_quad :: [Double] -> [Double] -> Double
cost_quad inputs goals = 0.5 * (sum (zipWith (\x y -> (x-y)^2) inputs goals))

-- partial derivative, with respect to the first argument (inputs)
cost_quad' :: Double -> Double -> Double
cost_quad' input goal = input - goal


-- last step (well, last complicated step, at least for now) is gradient descent.
-- the change in weight of each neuron is, as we have said, -(η*δ*a), where a is the output [=σ(z)] of whatever neuron this is receiving from.
-- the change in bias is -(η*δ).

eta :: Double
eta = 0.5

--                outputs (a)           error (δ)
descend_neuron :: [Double] -> Neuron -> Double -> Neuron
descend_neuron outputs (weights,bias) error = (zipWith (-) weights dweights,bias-dbias)
  where dweights = map (eta*error*) outputs
        dbias    = eta*error

--the arguments are backwards because I originally wrote this with inputs -> net -> goals, but then realized that this ordering would make it really easy to repeat
descend :: [Double] -> Net -> [Double] -> Net
descend inputs net goals = descend' inputs net errors
  where errors = backprop inputs net goals

descend' :: [Double] -> Net -> [[Double]] -> Net
descend' _ [] _ = []
descend' inputs (layer:layers) (errors:errorss) = (zipWith (descend_neuron inputs) layer errors):(descend' outputs layers errorss)
  where outputs = map (sigma . (zvar inputs)) layer

-- and here is a demonstration of our power
dem0 :: [Double] -> Net -> [Double] -> [[Double]]
dem0 inputs net outputs = map (compute inputs) $ iterate (flip (descend inputs) outputs) net

demo n = (demo [0.05,0.1] mazur [0.1,0.99]) !! n
