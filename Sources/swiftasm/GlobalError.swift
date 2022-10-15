enum GlobalError : Error {
    /* trying to use a deferred immediate which is not yet finalized
    */
    case immediateMissingValue   

    // generic invalid value passed
    case invalidValue(_ reason: String)
}