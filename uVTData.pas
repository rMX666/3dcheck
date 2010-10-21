unit uServiceDM;

interface

uses
  SysUtils;

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
  end;
  
implementation

end.
