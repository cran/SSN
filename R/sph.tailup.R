sph.tailup <- function(dist.hydro, weight, parsil = parsil, range1 = range1)
{
	parsil*(1 - 1.5*(dist.hydro/range1) +
		0.5*(dist.hydro/range1)^3)*(dist.hydro < range1)*weight
}

