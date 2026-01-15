package vecxt

object BoundsCheck:
  type BoundsCheck = Boolean

  object DoBoundsCheck:
    inline given yes: BoundsCheck = true
    inline given no: BoundsCheck = false
  end DoBoundsCheck
end BoundsCheck
