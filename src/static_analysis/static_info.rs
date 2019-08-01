
// Could include whether or not it is intialised / mutable etc if necessarry here
#[derive(Debug, Copy, Clone)]
pub enum StaticInfo {
    IVariable(bool), // is_initialised
    IFunction, // arity
    ILetBinding,
    IEmpty
}