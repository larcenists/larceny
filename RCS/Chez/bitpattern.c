unsigned bitpattern( i, f )
int i;
double f;
{
  if (i == 0)
    return *( (unsigned *) &f);
  else
    return *( (unsigned *) &f + 1);
}
