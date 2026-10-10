module Sample where

func :: Either (Maybe a) () -> Bool -> String
func a False = "One"
func (Left (Just a)) True = "Two"
func (Left Nothing) True = "Three"

funcBD = ["One", "Two", "Three"]
