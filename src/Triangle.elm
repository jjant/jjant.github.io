module Triangle exposing (..)


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    , t : Float
    }


perspective : Mat4
perspective =
    Mat4.makePerspective 45 1 0 0.01 1000
