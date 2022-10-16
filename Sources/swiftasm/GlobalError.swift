enum GlobalError : Error {
    /* trying to use a deferred immediate which is not yet finalized
    */
    case immediateMissingValue(_ reason: String)   

    // generic invalid value passed
    case invalidValue(_ reason: String)

    case invalidOperation(_ reason: String)
    case unexpected(_ reason: String)
}