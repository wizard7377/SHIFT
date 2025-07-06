module Extra.Decide where 

class Decide me where 
    decide :: me -> Bool
    badelem :: me 
