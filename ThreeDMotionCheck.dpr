program ThreeDMotionCheck;

uses
  Forms,
  uMain in 'uMain.pas' {fMain},
  uCameraDM in 'uCameraDM.pas' {fCameraDM: TDataModule},
  uConst in 'uConst.pas',
  uDebug in 'uDebug.pas',
  uParams in 'uParams.pas',
  uServiceDM in 'uServiceDM.pas' {fServiceDM: TDataModule},
  uMCPoint in 'uMCPoint.pas',
  u3DView in 'u3DView.pas' {f3DView},
  ifpii_MCFile in 'ifpii_MCFile.pas',
  ifpiir_MCFile in 'ifpiir_MCFile.pas',
  uVTData in 'uVTData.pas',
  uMCCounter in 'uMCCounter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfServiceDM, fServiceDM);
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfCameraDM, fCameraDM);
  Application.CreateForm(Tf3DView, f3DView);
  Application.Run;
end.
