# Test Script: break command
#
# NOTE: break is primarily tested in the context of the various loop commands.

test break-1.1 {break command} {
    # Returns the Break result code.
    break
} -break {}
