package com.young.sky.Entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.time.LocalDateTime;


@TableName("item_order")
@Data
public class Order {
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    private String username;
    private String itemName;
    private LocalDateTime CreateTime;
}

