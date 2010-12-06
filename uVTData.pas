unit uVTData;

interface

uses
  SysUtils, uMCCounter;

type
  PSceneTableData = ^TSceneTableData;
  TSceneTableData = record
    Caption: String;
    Value: String;
    PointChars: array of String;
  end;

  PSceneInfoData = ^TSceneInfoData;
  TSceneInfoData = record
    Caption: String;
    Value: String;
    CountMetaType: TCountParameterMetaType;
    CountType: TCountParameterType;
  end;
  
implementation

end.
