
import datetime
import pickle
import cloudpickle

def reformat_timestamp(timestamp):
    if len(timestamp)==10:
        return timestamp
    # Parse the input timestamp into a datetime object
    dt_object = datetime.strptime(timestamp, '%d-%b-%y')
    
    # Format the datetime object into the desired output format
    formatted_timestamp = dt_object.strftime('%Y-%m-%d')
    
    return formatted_timestamp


def save_trace(path, trace):
    dict_to_save={"trace": trace}
    with open(path , 'wb') as buff:
        cloudpickle.dump(dict_to_save, buff)

def load_trace(path):
    with open(path , 'rb') as buff:
        return cloudpickle.load(buff)["trace"]
    


def min_distance_to_interval(interval, critical_value=0):
    # Sort interval endpoints
    a, b = sorted(interval)
    
    # If value is within the interval, distance is 0
    if a <= critical_value <= b:
        return 0, "neutral"
    
    # Compute distance to the nearest endpoint
    distance_to_a = abs(critical_value - a)
    distance_to_b = abs(critical_value - b)

    if a > critical_value:
        side="increase"
    elif b < critical_value:
        side="decrease"
    
    # Return the minimum distance
    return min(distance_to_a, distance_to_b), side