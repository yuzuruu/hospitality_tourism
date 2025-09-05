import cv2
import csv
import os
from ultralytics import YOLO

# Load YOLO model
model = YOLO(model="./model/yolov8x.pt")

# Folder containing video files
video_folder = "./data/detect/"
output_folder = "./detection_results/"
os.makedirs(output_folder, exist_ok=True)

# Process each video file in the folder
for video_file in os.listdir(video_folder):
    if not video_file.endswith(".mp4"):  # Process only .mp4 files
        continue
    
    video_path = os.path.join(video_folder, video_file)
    output_csv = os.path.join(output_folder, f"{os.path.splitext(video_file)[0]}_results.csv")
    
    # Open video file to extract frame timestamps
    cap = cv2.VideoCapture(video_path)
    fps = cap.get(cv2.CAP_PROP_FPS)  # Frames per second
    frame_time_ms = 1000 / fps if fps > 0 else 0  # Time per frame in milliseconds
    
    # Ensure CSV file has headers if it doesn't exist
    if not os.path.exists(output_csv):
        with open(output_csv, mode="w", newline="") as f:
            writer = csv.writer(f)
            writer.writerow(["timestamp_ms", "frame_number", "class_id", "class_name", "confidence", "x", "y", "width", "height"])
    
    # Process video with YOLO
    results = model.predict(
        source=video_path,
        show=False,
        stream=True,  # Stream results frame-by-frame
        save=True,
        save_txt=True,
        save_conf=True,
        imgsz=640,
        conf=0.35,
        line_width=3,
    )
    
    print(f"---START PROCESSING: {video_file}---")
    frame_number = 0  # Track frame count
    
    for res in results:
        ret, frame = cap.read()
        if not ret:
            break  # Stop if video ends

        frame_number += 1  # Increment frame count
        timestamp_ms = int(frame_number * frame_time_ms)  # Calculate timestamp in milliseconds

        print(f"Frame: {frame_number}, Timestamp: {timestamp_ms} ms")
        print(f"Number of boxes = {len(res.boxes)}")
        
        # Open CSV file to append detection results
        with open(output_csv, mode="a", newline="") as f:
            writer = csv.writer(f)
            
            for box in res.boxes:
                conf = box.conf[0].item()  # Confidence score
                cls = int(box.cls[0].item())  # Class ID
                clsnm = res.names[cls]  # Class name
                pos = box.xywh[0]  # Bounding box coordinates
                
                # Print detection details
                print(f"Class = {cls}:{clsnm}, Confidence = {conf:.5f}")
                print(f"Position = X:{pos[0].item()} Y:{pos[1].item()} Width:{pos[2].item()} Height:{pos[3].item()}")
                
                # Save detection details to CSV
                writer.writerow([timestamp_ms, frame_number, cls, clsnm, conf,
                                 pos[0].item(), pos[1].item(), pos[2].item(), pos[3].item()])
        
        # Display the annotated frame
        annotated_frame = res.plot(line_width=1)
        cv2.imshow("Detection", annotated_frame)

        # Press 'q' to quit
        if cv2.waitKey(1) & 0xFF == ord('q'):
            break
    
    cap.release()
    print(f"---END PROCESSING: {video_file}---")

cv2.destroyAllWindows()
