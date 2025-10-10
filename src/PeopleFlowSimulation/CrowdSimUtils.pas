unit CrowdSimUtils;

interface

type
    Vector2D = packed record // 8 bytes
        x : Single; // Bytes# 0-3
        y : Single; // Bytes# 4-7
        procedure Initialize;
        end;

    Vector3D = packed record  // 12 bytes
        x : Single; // Bytes# 0-3
        y : Single; // Bytes# 4-7
        z : Single; // Bytes# 8-11
        procedure Initialize;
        end;

    AxisType = (X_AXIS, Y_AXIS, Z_AXIS);


implementation

{ Vector2D }

procedure Vector2D.Initialize;
    begin
    x := 0.0;
    y := 0.0;
    end;

{ Vector3D }

procedure Vector3D.Initialize;
    begin
    x := 0.0;
    y := 0.0;
    z := 0.0;
    end;

end.
