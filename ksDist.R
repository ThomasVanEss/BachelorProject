ksDist = function (dist1,dist2) {
  max = 0; sum1 = 0; sum2 = 0; n = sum(dist1)
  for (i in 1:min(length(dist1),length(dist2))) {
    sum1 = sum1 + dist1[i]/n; sum2 = sum2 + dist2[i]/n;
    dif = abs(sum1 - sum2);
    if (dif  > max) {max = dif}
  }
  return (max)
}