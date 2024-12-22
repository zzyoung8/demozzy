package com.young.sky.Controller;

import com.young.sky.Service.PayService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping("/pay")
public class PayController {

    @Autowired
    private PayService payService;

    @GetMapping("/{id}")
    public boolean payTicket(@PathVariable Integer id){
        return payService.payTicket(id);
    }
}
