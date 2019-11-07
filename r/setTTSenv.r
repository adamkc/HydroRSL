setTTSenv <-
function (name, value, env) 
{
    if (missing(env)) 
        env <- getTTSenv()
    env[name, "value"] <- value
    write.table(env, "TTSenvironment.txt", sep = "=", col.names = F, 
        quote = F)
}
