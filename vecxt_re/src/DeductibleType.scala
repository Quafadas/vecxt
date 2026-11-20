package vecxt_re

enum DeductibleType:
  case Retention // A hard deductible - i.e., the ceded amount is zero until the retention is exceeded, then the ceded amount is the excess over the retention, up to the limit.
  case Franchise // A soft deductible - i.e., the ceded amount is zero until the franchise is exceeded or matched, then the ceded amount is the full amount up to the limit + franchise point.
  case ReverseFranchise // A reverse franchise - i.e., the ceded amount is paid until the reverse franchise is exceeded, then the ceded amount is zero.
end DeductibleType
