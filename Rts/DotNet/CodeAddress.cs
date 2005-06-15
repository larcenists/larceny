using System;
using Scheme.Rep;

namespace Scheme.RT
{

public sealed class CodeAddress
{
  public readonly CodeVector code;
  public readonly int label;

  public CodeAddress (CodeVector code)
  {
    this.code = code;
    this.label = 0;
  }

  public CodeAddress (CodeVector code, int label)
  {
    this.code = code;
    this.label = label;
  }

  public CodeAddress (Procedure p)
  {
    this.code = p.entrypoint;
    this.label = 0;
  }

}

}
